#if defined(_WIN32)
#include <Windows.h>
#include "DriverStorePath.h"
#else
#include <dlfcn.h>
#include <stdarg.h>
#include <string.h>
#endif
#include <iostream>
#include <sstream>
#include <string>

#include "metrics_discovery_api.h"

#include "mdapi_wrapper.hpp"


void metric_map::append_val(
  const char *metric,
  const char *units,
  bool is_info,
  metric_val::metric_val_type ty,
  metric_val::mval v)
{
  metric_val *mp = nullptr;
  for (metric_val &mv : columns) {
    if (mv.metric == metric) {
      mp = &mv;
      break;
    }
  }
  if (!mp) {
    columns.emplace_back();
    mp = &columns.back();
    mp->type = ty;
    mp->metric = metric;
    mp->units = units ? units : "";
    mp->is_info = is_info;
  }
  if (mp->type != ty) {
    std::cerr << "type mismatch in counter accumulation\n";
    exit(EXIT_FAILURE);
  }
  mp->values.emplace_back(v);
}

// Override to force load from explicit DLL
const char *MDAPI_FULLPATH = nullptr;

static void *load_mdapi_lib();
static void *load_sym(void *lib, const char *sym);

#if defined(_WIN32)
static const wchar_t *MDAPI_LIB =
    sizeof(void *) == 8 ? L"igdmd64.dll" : L"igdmd32.dll";

static void *load_mdapi_lib()
{
  //  (void*)LoadLibraryA(MDAPI_FULLPATH)
  return LoadDynamicLibrary(MDAPI_LIB);
}
static void *load_sym(void *lib, const char *sym)
{
  return GetProcAddress((HMODULE)lib, sym);
}

#elif defined(__linux__) || defined(__FreeBSD__) || defined(__APPLE__)
#if defined(__linux__) || defined(__FreeBSD__)
static const char *MDAPI_LIB = "libigdmd.so";
#else
static const char *MDAPI_LIB = "libigdmd.dylib";
#endif

static void *load_mdapi_lib()
{
  void *ret = NULL;
  if (MDAPI_FULLPATH) {
    ret = dlopen(metricsLibraryName.c_str(), RTLD_LAZY | RTLD_LOCAL);
  } else {
    if (ret == NULL) {
      ret = dlopen(MDAPI_LIB, RTLD_LAZY | RTLD_LOCAL);
    }
#if !defined(__APPLE__)
    if (ret == NULL) {
      // old alternate name, may eventually be removed
      ret = dlopen("libmd.so", RTLD_LAZY | RTLD_LOCAL);
    }
#endif
  }
  return ret;
}

#else
#error "Invalid host platform"
#endif

template<
    typename T1,
    typename T2,
    typename T3 = const char *,
    typename T4 = const char *,
    typename T5 = const char *,
    typename T6 = const char *,
    typename T7 = const char *>
static std::string
cat(T1 t1, T2 t2, T3 t3 = "", T4 t4 = "", T5 t5 = "", T6 t6 = "", T7 t7 = "")
{
  std::stringstream ss;
  ss << t1 << t2 << t3 << t4 << t5 << t6 << t7;
  return ss.str();
}
static std::string fmt_hex(uint64_t x)
{
  std::stringstream ss;
  ss << "0x" << std::uppercase << std::hex << x;
  return ss.str();
};


static const uint32_t API_MASK      = MetricsDiscovery::API_TYPE_OCL;
static const uint32_t CATEGORY_MASK = 0xFFFFFFFF;

struct mdapi_wrapper_impl {
  std::string error;

  void *lib = nullptr;

  MetricsDiscovery::OpenMetricsDevice_fn         OpenMetricsDevice  = nullptr;
  MetricsDiscovery::CloseMetricsDevice_fn        CloseMetricsDevice = nullptr;
  MetricsDiscovery::OpenMetricsDeviceFromFile_fn OpenMetricsDeviceFromFile =
      nullptr;
  //
  MetricsDiscovery::IMetricsDevice_1_5       *m_device = nullptr;
  MetricsDiscovery::TMetricsDeviceParams_1_0 *m_device_params = nullptr;
  //
  MetricsDiscovery::IConcurrentGroup_1_1 *m_conc_group = nullptr;
  MetricsDiscovery::IMetricSet_1_1       *m_metric_set = nullptr;
  MetricsDiscovery::TMetricSetParams_1_0 *m_metric_set_params = nullptr;
  //
  bool active = false;

  mdapi_wrapper_impl()
  {
    lib = load_mdapi_lib();
    if (!lib) {
      error = "could not load lib";
      return;
    }

    CloseMetricsDevice = (MetricsDiscovery::CloseMetricsDevice_fn)load_sym(
        lib, "CloseMetricsDevice");
    if (CloseMetricsDevice == NULL) {
      close_with_error("Couldn't get pointer to CloseMetricsDevice");
      return;
    }

    OpenMetricsDevice = (MetricsDiscovery::OpenMetricsDevice_fn)load_sym(
        lib, "OpenMetricsDevice");
    if (OpenMetricsDevice == NULL) {
      close_with_error("Couldn't get pointer to OpenMetricsDevice");
      return;
    }

    OpenMetricsDeviceFromFile =
        (MetricsDiscovery::OpenMetricsDeviceFromFile_fn)load_sym(
            lib, "OpenMetricsDeviceFromFile");
    if (OpenMetricsDeviceFromFile == NULL) {
      close_with_error("Couldn't get pointer to OpenMetricsDeviceFromFile");
      return;
    }

    // res = OpenMetricsDeviceFromFile(metricsFileName.c_str(), (void*)"",
    // &m_device);
    MetricsDiscovery::TCompletionCode res = OpenMetricsDevice(&m_device);
    if (res != MetricsDiscovery::CC_OK) {
      close_with_error(cat("OpenMetricsDevice failed ", (int)res));
      return;
    }

    m_device_params = m_device->GetParams();
    if (!m_device_params) {
      close_with_error("IMetricsDevice_1_5::GetParams() returned nullptr");
      return;
    }
  } // mdapi_wrapper_impl

  ~mdapi_wrapper_impl() {
    close_lib();
  }

  void close_lib()
  {
    if (CloseMetricsDevice && m_device) {
      CloseMetricsDevice(m_device);
      m_device = nullptr;
    }
#ifdef _WIN32
    (void)FreeLibrary((HMODULE)lib);
#else
    (void)dlclose(lib);
#endif
    lib = nullptr;
  }

  std::string get_version() {
    std::stringstream ss;
    ss <<
        "Headers: v" <<
        MetricsDiscovery::MD_API_MAJOR_NUMBER_CURRENT << "." <<
        MetricsDiscovery::MD_API_MINOR_NUMBER_CURRENT << "." <<
        MD_API_BUILD_NUMBER_CURRENT <<
        "; Library: v";
    if (m_device_params) {
      ss << m_device_params->Version.MajorNumber << "."
         << m_device_params->Version.MinorNumber << "."
         << m_device_params->Version.BuildNumber;
    } else {
      ss << "???";
    }
    return ss.str();
  }

  template<
      typename T1,
      typename T2 = const char *,
      typename T3 = const char *,
      typename T4 = const char *,
      typename T5 = const char *,
      typename T6 = const char *,
      typename T7 = const char *>
  bool close_with_error(
      T1 t1,
      T2 t2 = "",
      T3 t3 = "",
      T4 t4 = "",
      T5 t5 = "",
      T6 t6 = "",
      T7 t7 = "")
  {
    std::stringstream ss;
    ss << t1 << t2 << t3 << t4 << t5 << t6 << t7;
    if (m_metric_set) {
      deactivate();
      m_conc_group = nullptr;
      m_metric_set = nullptr;
    }

    close_lib();
    error = ss.str();
    return false;
  }

  using metric_set_pair = std::pair<
      MetricsDiscovery::IConcurrentGroup_1_1 *,
      MetricsDiscovery::IMetricSet_1_1 *>;

  metric_set_pair find_metric_set(const char *target_metric_cstr)
  {
    MetricsDiscovery::TMetricsDeviceParams_1_0 *deviceParams =
        m_device->GetParams();
    if (deviceParams == nullptr) {
      close_with_error("TMetricsDeviceParams_1_0 is null");
      return metric_set_pair(nullptr, nullptr);
    }

    const std::string target_metric = target_metric_cstr;
    for (uint32_t cg = 0; cg < deviceParams->ConcurrentGroupsCount; cg++) {
      MetricsDiscovery::IConcurrentGroup_1_1 *group =
          m_device->GetConcurrentGroup(cg);
      MetricsDiscovery::TConcurrentGroupParams_1_0 *group_params =
          group->GetParams();
      if (group_params) {
        for (uint32_t ms_ix = 0; ms_ix < group_params->MetricSetsCount;
             ms_ix++) {
          MetricsDiscovery::IMetricSet_1_1 *ms = group->GetMetricSet(ms_ix);
          if (!ms) {
            close_with_error(
                "IConcurrentGroup_1_1::GetMetricSet returned nullptr");
            return metric_set_pair(nullptr, nullptr);
          }
          MetricsDiscovery::TMetricSetParams_1_0 *ms_params = ms->GetParams();
          if (!ms_params) {
            close_with_error("IMetricSet_1_1::GetParams return nullptr");
            return metric_set_pair(nullptr, nullptr);
          }
          if ((ms_params->ApiMask & API_MASK) &&
              (ms_params->CategoryMask & CATEGORY_MASK) &&
              (ms_params->SymbolName == target_metric)) {
            return metric_set_pair(group, ms);
          }
        } // for metric sets
      }
    } // for conc group
    return metric_set_pair(nullptr, nullptr); // not found
  } // find_metric_set()

  bool list_metric_sets(std::vector<ms_info> &mss)
  {
    MetricsDiscovery::TMetricsDeviceParams_1_0 *device_params =
        m_device->GetParams();
    if (device_params == nullptr) {
      close_with_error("TMetricsDeviceParams_1_0 is null");
      return false;
    }

    for (uint32_t cg = 0; cg < device_params->ConcurrentGroupsCount; cg++) {
      MetricsDiscovery::IConcurrentGroup_1_1 *group =
          m_device->GetConcurrentGroup(cg);
      MetricsDiscovery::TConcurrentGroupParams_1_0 *group_params =
          group->GetParams();
      if (group_params) {
        for (uint32_t ms_ix = 0; ms_ix < group_params->MetricSetsCount;
             ms_ix++) {
          MetricsDiscovery::IMetricSet_1_1 *m_set = group->GetMetricSet(ms_ix);
          if (!m_set) {
            return close_with_error("GetMetricSet returned null");
          }
          MetricsDiscovery::TMetricSetParams_1_0 *m_set_params =
              m_set->GetParams();
          if (!m_set_params) {
            return close_with_error("set params are null");
          }
          if ((m_set_params->ApiMask & API_MASK)) {
            mss.emplace_back(group_params->SymbolName, m_set_params->SymbolName);
            auto &ms = mss.back();

            for (uint32_t mi = 0 ; mi < m_set_params->MetricsCount; mi++) {
              MetricsDiscovery::IMetric_1_0 *m = m_set->GetMetric(mi);
              MetricsDiscovery::TMetricParams_1_0 *m_set_params = m->GetParams();
              if (m_set_params->ApiMask & API_MASK) {
                std::stringstream ss;
                ss << m_set_params->SymbolName;
                ss << ":";
                switch (m_set_params->ResultType) {
                case MetricsDiscovery::RESULT_UINT32: ss << "RESULT_UINT32"; break;
                case MetricsDiscovery::RESULT_UINT64: ss << "RESULT_UINT64"; break;
                case MetricsDiscovery::RESULT_BOOL: ss << "RESULT_BOOL"; break;
                case MetricsDiscovery::RESULT_FLOAT: ss << "RESULT_FLOAT"; break;
                default: ss << "?"; break;
                }
                ss << " => " << m_set_params->ShortName << " (in "
                   << m_set_params->MetricResultUnits << "); " << m_set_params->LongName;
                ms.metrics.emplace_back(ss.str());
              }
            } // metrics
            for (uint32_t ii = 0 ; ii < m_set_params->InformationCount; ii++) {
              MetricsDiscovery::IInformation_1_0 *i = m_set->GetInformation(ii);
              MetricsDiscovery::TInformationParams_1_0 *ip = i->GetParams();
              if (ip->ApiMask & API_MASK) {
                std::stringstream ss;
                ss << ip->SymbolName << ":";
                switch (ip->InfoType) {
                case MetricsDiscovery::INFORMATION_TYPE_REPORT_REASON: ss << "REPORT_REASON"; break;
                case MetricsDiscovery::INFORMATION_TYPE_VALUE: ss << "VALUE"; break;
                case MetricsDiscovery::INFORMATION_TYPE_FLAG: ss << "FLAG"; break;
                case MetricsDiscovery::INFORMATION_TYPE_TIMESTAMP: ss << "TIMESTAMP"; break;
                case MetricsDiscovery::INFORMATION_TYPE_CONTEXT_ID_TAG: ss << "CONTEXT_ID_TAG"; break;
                case MetricsDiscovery::INFORMATION_TYPE_SAMPLE_PHASE: ss << "SAMPLE_PHASE"; break;
                case MetricsDiscovery::INFORMATION_TYPE_GPU_NODE: ss << "GPU_NODE"; break;
                default: ss << "?"; break;
                }
                ss << " ==> " << ip->ShortName;
                if (ip->InfoUnits) {
                  ss << " (in " << ip->InfoUnits << ")";
                }
                ss << "; " << ip->LongName;
                ms.metrics.emplace_back(ss.str());
              }
            }
          }
        } // for metric sets
      }
    } // for conc group

    return true;
  } // list_metric_sets()

  bool bind(const char *metric_set_name)
  {
    if (m_metric_set) {
      return close_with_error("activate: metric set already active");
    }
    auto ms = find_metric_set(metric_set_name);
    if (ms.first == nullptr) {
      return false;
    }
    m_conc_group = ms.first;
    m_metric_set = ms.second;

    auto ret = m_metric_set->SetApiFiltering(API_MASK);
    if (ret != MetricsDiscovery::CC_OK) {
      return close_with_error(
          "bind(): SetApiFiltering(OCL) returned ", (int)ret);
    }

    // only call GetParams *after* SetApiFiltering since the latter
    // affects the behavior of the former
    m_metric_set_params = m_metric_set->GetParams();
    if (!m_metric_set_params) {
      return close_with_error(
          "bind(): IMetricSet_1_1::GetParams() returned null");
    }

    return true;
  }

  uint32_t get_configuration() {
    if (!m_metric_set_params) {
      close_with_error("get_configuration: metric set already active");
      return 0;
    }
    return m_metric_set_params->ApiSpecificId.OCL;
  }

  uint32_t get_query_report_size() {
    if (!m_metric_set_params) {
      close_with_error("get_configuration: metric set already active");
      return 0;
    }
    return m_metric_set_params->QueryReportSize;
  }

  bool activate()
  {
    if (active) {
      return close_with_error("activate: metric set already active");
    }

    MetricsDiscovery::TCompletionCode res = m_metric_set->Activate();
    if (res != MetricsDiscovery::CC_OK) {
      return close_with_error("IMetricSet_1_1::Active() failed, ", (int)res);
    }

    active = true;

    return true;
  } // activate()

  bool deactivate()
  {
    if (!m_metric_set) {
      return close_with_error("deactivate: metric set not bound");
    } else if (!active) {
      return true;
    }

    MetricsDiscovery::TCompletionCode res = m_metric_set->Deactivate();
    active = false;
    if (res != MetricsDiscovery::CC_OK) {
      return close_with_error("IMetricSet_1_1::Deactivate() failed", (int)res);
    }

    return true;
  } // deactivate()

  bool parse_counter_buffer(metric_map &mm, void *buf) {
    if (!m_metric_set) {
      return close_with_error("deactivate: metric set not bound");
    }
    std::vector<MetricsDiscovery::TTypedValue_1_0> vals;
    vals.resize(
        m_metric_set_params->MetricsCount +
        m_metric_set_params->InformationCount);

    const bool     context_filtering = false;
    uint32_t       out_report_count  = 0;
    const uint32_t vals_out_size =
        ((uint32_t)vals.size() * sizeof(MetricsDiscovery::TTypedValue_1_0));
    const uint32_t query_report_size = get_query_report_size();

    auto res = m_metric_set->CalculateMetrics(
        (const unsigned char *)buf,
        query_report_size,
        vals.data(),
        vals_out_size,
        &out_report_count,
        context_filtering);
    if (res != MetricsDiscovery::CC_OK) {
      return close_with_error(
          "IMetricSet_1_1::CalculateMetrics() failed ", (int)res);
    } else if (out_report_count != 1) {
      return close_with_error("expected one report");
    }

    unsigned curr_metric = 0;
    for (unsigned mi = 0; mi < m_metric_set_params->MetricsCount; mi++) {
      if (curr_metric >= vals.size()) {
        return close_with_error("insufficient values in report");
      }
      const auto &v = vals[curr_metric++];
      MetricsDiscovery::IMetric_1_0 *m = m_metric_set->GetMetric(mi);
      MetricsDiscovery::TMetricParams_1_0 *mp = m->GetParams();
      switch (v.ValueType) {
      case MetricsDiscovery::VALUE_TYPE_UINT32:
        if (mp->ResultType != MetricsDiscovery::RESULT_UINT32) {
          return close_with_error(
              "parse_counter_buffer(): ",
              mp->SymbolName,
              ": type mismatch on metric");
        }
        mm.append(mp->SymbolName, mp->MetricResultUnits, false, v.ValueUInt32);
        break;
      case MetricsDiscovery::VALUE_TYPE_UINT64:
        if (mp->ResultType != MetricsDiscovery::RESULT_UINT64) {
          return close_with_error(
              "parse_counter_buffer(): ",
              mp->SymbolName,
              ": type mismatch on metric");
        }
        mm.append(mp->SymbolName, mp->MetricResultUnits, false, v.ValueUInt64);
        break;
      case MetricsDiscovery::VALUE_TYPE_FLOAT:
        if (mp->ResultType != MetricsDiscovery::RESULT_FLOAT) {
          return close_with_error(
              "parse_counter_buffer(): ",
              mp->SymbolName,
              ": type mismatch on metric");
        }
        mm.append(mp->SymbolName, mp->MetricResultUnits, false, v.ValueFloat);
        break;
      case MetricsDiscovery::VALUE_TYPE_BOOL:
        if (mp->ResultType != MetricsDiscovery::RESULT_BOOL) {
          return close_with_error(
              "parse_counter_buffer(): ",
              mp->SymbolName,
              ": type mismatch on metric");
        }
        mm.append(mp->SymbolName, mp->MetricResultUnits, false, v.ValueBool);
        break;
      case MetricsDiscovery::VALUE_TYPE_CSTRING:
        if (true) {
          return close_with_error(
              "parse_counter_buffer(): ",
              mp->SymbolName,
              ": type unexpected string");
        }
        mm.append(mp->SymbolName, mp->MetricResultUnits, false, v.ValueCString);
        break;
      default:
        close_with_error(
            "parse_counter_buffer(): unhandled type ", v.ValueType);
      }
    } // metrics
    for (unsigned ii = 0; ii < m_metric_set_params->InformationCount; ii++) {
      if (curr_metric >= vals.size()) {
        return close_with_error("insufficient values in report");
      }
      const auto &v = vals[curr_metric++];
      MetricsDiscovery::IInformation_1_0 *i = m_metric_set->GetInformation(ii);
      MetricsDiscovery::TInformationParams_1_0 *ip = i->GetParams();
      switch (v.ValueType) {
      case MetricsDiscovery::VALUE_TYPE_UINT32:
        mm.append(ip->SymbolName, ip->InfoUnits, true, v.ValueUInt32);
        break;
      case MetricsDiscovery::VALUE_TYPE_UINT64:
        mm.append(ip->SymbolName, ip->InfoUnits, true, v.ValueUInt64);
        break;
      case MetricsDiscovery::VALUE_TYPE_FLOAT:
        mm.append(ip->SymbolName, ip->InfoUnits, true, v.ValueFloat);
        break;
      case MetricsDiscovery::VALUE_TYPE_BOOL:
        mm.append(ip->SymbolName, ip->InfoUnits, true, v.ValueBool);
        break;
      case MetricsDiscovery::VALUE_TYPE_CSTRING:
        mm.append(ip->SymbolName, ip->InfoUnits, true, v.ValueCString);
        break;
      default:
        close_with_error(
            "parse_counter_buffer(): unhandled type ", v.ValueType);
      }
    } // information

    if (curr_metric != vals.size()) {
      return close_with_error("wrong number of values in report");
    }
    // TODO: I could get max values: ((MetricsDiscovery::IMetricSet_1_5*)m_MetricSet)->CalculateMetrics

    return true;
  }
}; // mdapi_wrapper_impl

///////////////////////////////////////////////////////////////////////////////
// public API
mdapi_lib::mdapi_lib() : impl(new mdapi_wrapper_impl())
{
}

mdapi_lib::~mdapi_lib()
{
  if (impl)
    delete (struct mdapi_wrapper_impl *)impl;
  impl = nullptr;
}

std::string mdapi_lib::get_error() const
{
  if (impl) {
    return ((struct mdapi_wrapper_impl *)impl)->error;
  } else {
    return "not loaded";
  }
}

std::string mdapi_lib::get_version() const
{
  if (impl) {
    return ((struct mdapi_wrapper_impl *)impl)->get_version();
  } else {
    return "?";
  }
}

bool mdapi_lib::is_loaded() const
{
  if (!impl) {
    return false;
  }
  return ((struct mdapi_wrapper_impl *)impl)->lib != nullptr;
}

std::vector<ms_info> mdapi_lib::list_metric_sets() const
{
  if (!impl) {
    return std::vector<ms_info>();
  }

  std::vector<ms_info> msis;
  if (!((struct mdapi_wrapper_impl *)impl)->list_metric_sets(msis)) {
    return std::vector<ms_info>();
  }
  return msis;
}

uint32_t mdapi_lib::get_configuration() const {
  if (!impl) {
    return false;
  }
  return ((struct mdapi_wrapper_impl *)impl)->get_configuration();
}
uint32_t mdapi_lib::get_query_report_size() const {
  if (!impl) {
    return false;
  }
  return ((struct mdapi_wrapper_impl *)impl)->get_query_report_size();
}

bool mdapi_lib::bind(const char *metric_set_name) {
  if (!impl) {
    return false;
  }
  return ((struct mdapi_wrapper_impl *)impl)->bind(metric_set_name);
}
bool mdapi_lib::activate()
{
  if (!impl) {
    return false;
  }
  return ((struct mdapi_wrapper_impl *)impl)->activate();
}

bool mdapi_lib::deactivate()
{
  if (!impl) {
    return false;
  }
  return ((struct mdapi_wrapper_impl *)impl)->deactivate();
}

bool mdapi_lib::parse_counter_buffer(metric_map &mm, void *buf)
{
  if (!impl) {
    return false;
  }
  return ((struct mdapi_wrapper_impl *)impl)->parse_counter_buffer(mm, buf);
}

void list_mdapi_metrics(int verbosity, std::vector<std::string> ms_filts_inp)
{
  auto to_lower = [&](std::string s) {
    std::stringstream ss;
    for (unsigned i = 0; i < (unsigned)s.length(); i++)
      ss << (char)std::tolower(s[i]);
    return ss.str();
  };

  std::vector<std::pair<std::string,std::string>> ms_filts;
  for (auto s : ms_filts_inp) {
    auto ix = s.find('.');
    std::string msname, mname;
    if (ix != std::string::npos) {
      msname = s.substr(0, ix);
      mname = s.substr(ix + 1);
    } else {
      msname = s;
    }
    ms_filts.emplace_back(to_lower(msname), to_lower(mname));
  }

  auto matches_metric_set_filter = [&](std::string ms) {
    if (ms_filts.empty())
      return true;
    auto ms_lower = to_lower(ms);
    for (auto [filt_ms,_] : ms_filts) {
      auto ix = filt_ms.find(ms_lower);
      if (filt_ms.empty() || ms_lower.find(filt_ms) != std::string::npos)
        return true;
    }
    return false;
  };
  auto matches_filter = [&](std::string ms, std::string m) {
    if (ms_filts.empty())
      return true;
    auto m_lower = to_lower(m);
    auto ms_lower = to_lower(ms);
    for (auto [filt_ms,filt_m] : ms_filts)
      if ((filt_ms.empty() || ms_lower.find(filt_ms) != std::string::npos) &&
          (filt_m.empty() || m_lower.find(filt_m) != std::string::npos))
        return true;
    return false;
  };

  std::ostream &os = std::cout;

  mdapi_lib md_lib;
  if (!md_lib.is_loaded()) {
    std::cerr << "failed to load MDAPI library (" << md_lib.get_error() << ")\n";
    return;
  }
  const auto *mwi = (const mdapi_wrapper_impl *)md_lib.impl;
  if (!mwi) {
    std::cerr << "failed to load MDAPI library (" << md_lib.get_error() << ")\n";
    return;
  }

  constexpr const char *ANSI_RESET {"\033[0m"};
  constexpr const char *ANSI_COLOR_DBLUE {"\033[38;2;10;120;190m"};
  constexpr const char *ANSI_COLOR_BLUE {"\033[38;2;20;153;245m"};
  constexpr const char *ANSI_COLOR_LBLUE {"\033[38;2;60;170;255m"};
  constexpr const char *ANSI_COLOR_TYPE {"\033[38;2;236;166;57m"};

  auto metric_set_name = [&](const char *s) {
    std::stringstream ss;
    ss << ANSI_COLOR_DBLUE << s << ANSI_RESET;
    return ss.str();
  };
  auto metric_name = [&](const char *s) {
    std::stringstream ss;
    ss << ANSI_COLOR_BLUE << s << ANSI_RESET;
    return ss.str();
  };
  auto info_name = [&](const char *s) {
    std::stringstream ss;
    ss << ANSI_COLOR_LBLUE << s << ANSI_RESET;
    return ss.str();
  };
  auto type = [&](const char *s) {
    std::stringstream ss;
    ss << ANSI_COLOR_TYPE << s << ANSI_RESET;
    return ss.str();
  };

  MetricsDiscovery::TMetricsDeviceParams_1_0 *device_params =
      mwi->m_device->GetParams();
  if (device_params == nullptr) {
    std::cerr << "nullptr device params\n";
    return;
  }
  os << "MDAPI Versions " << md_lib.get_version() << "\n";

  // Group
  // Metric
  for (uint32_t cg = 0; cg < device_params->ConcurrentGroupsCount; cg++) {
    MetricsDiscovery::IConcurrentGroup_1_1 *group =
        mwi->m_device->GetConcurrentGroup(cg);
    MetricsDiscovery::TConcurrentGroupParams_1_0 *group_params =
        group->GetParams();
    if (!group_params) {
      continue;
    }
    for (uint32_t ms_ix = 0; ms_ix < group_params->MetricSetsCount;
          ms_ix++) {
      MetricsDiscovery::IMetricSet_1_1 *m_set = group->GetMetricSet(ms_ix);
      if (!m_set) {
        std::cerr << "GetMetricSet returned null\n";
        return;
      }
      MetricsDiscovery::TMetricSetParams_1_0 *m_set_params =
          m_set->GetParams();
      if (!m_set_params) {
        std::cerr << "set params are null\n";
        return;
      } else if ((m_set_params->ApiMask & API_MASK) == 0) {
        continue;
      } else if (!matches_metric_set_filter(m_set_params->SymbolName)) {
        continue;
      }

      os << "== MetricSet(" << metric_set_name(m_set_params->SymbolName) << ")\n";
      if (verbosity >= 1)
        os << "   " << m_set_params->ShortName << "\n";

      for (uint32_t mi = 0 ; mi < m_set_params->MetricsCount; mi++) {
        MetricsDiscovery::IMetric_1_0 *m = m_set->GetMetric(mi);
        MetricsDiscovery::TMetricParams_1_0 *m_params = m->GetParams();
        if ((m_params->ApiMask & API_MASK) == 0) {
          continue;
        } else if (!matches_filter(
                       m_set_params->SymbolName, m_params->SymbolName)) {
          continue;
        }
        os << "  * Metric(" << metric_name(m_params->SymbolName) << "):";
        switch (m_params->ResultType) {
        case MetricsDiscovery::RESULT_UINT32: os << type("UINT32"); break;
        case MetricsDiscovery::RESULT_UINT64: os << type("UINT64"); break;
        case MetricsDiscovery::RESULT_BOOL: os << type("BOOL"); break;
        case MetricsDiscovery::RESULT_FLOAT: os << type("FLOAT"); break;
        default:
          os << "TMetricResultType(" << m_params->ResultType << ")";
          break;
        }
        os << " ==> " << m_params->ShortName << " (in "
            << type(m_params->MetricResultUnits) << ")\n";
        if (verbosity >= 1)
          os << "    " << m_params->LongName << "\n";
      } // metrics
      for (uint32_t ii = 0 ; ii < m_set_params->InformationCount; ii++) {
        MetricsDiscovery::IInformation_1_0 *i = m_set->GetInformation(ii);
        MetricsDiscovery::TInformationParams_1_0 *ip = i->GetParams();
        if ((ip->ApiMask & API_MASK) == 0) {
          continue;
        } else if (!matches_metric_set_filter(m_set_params->SymbolName)) {
          continue;
        }
        os << "  * Information(" << info_name(ip->SymbolName) << "):";
        switch (ip->InfoType) {
        case MetricsDiscovery::INFORMATION_TYPE_REPORT_REASON:
          os << type("REPORT_REASON");
          break;
        case MetricsDiscovery::INFORMATION_TYPE_VALUE:
          os << type("VALUE");
          break;
        case MetricsDiscovery::INFORMATION_TYPE_FLAG: os << type("FLAG"); break;
        case MetricsDiscovery::INFORMATION_TYPE_TIMESTAMP:
          os << type("TIMESTAMP");
          break;
        case MetricsDiscovery::INFORMATION_TYPE_CONTEXT_ID_TAG:
          os << type("CONTEXT_ID_TAG");
          break;
        case MetricsDiscovery::INFORMATION_TYPE_SAMPLE_PHASE:
          os << type("SAMPLE_PHASE");
          break;
        case MetricsDiscovery::INFORMATION_TYPE_GPU_NODE:
          os << type("GPU_NODE");
          break;
        default: os << "TInformationType(" << ip->InfoType << ")?"; break;
        }
        os << " ==> " << ip->ShortName;
        if (ip->InfoUnits) {
          os << " (in " << type(ip->InfoUnits) << ")";
        }
        os << "\n";
        if (verbosity >= 1)
          os << "    " << ip->LongName << "\n";
      } // informations
    } // for metric sets
  } // for conc group
}
