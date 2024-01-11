#ifndef MDAPI_WRAPPER_HPP
#define MDAPI_WRAPPER_HPP

#include <cstdint>
#include <string>
#include <vector>
#include <variant>

#ifndef CL_PROFILING_COMMAND_PERFCOUNTERS_INTEL
#define CL_PROFILING_COMMAND_PERFCOUNTERS_INTEL 0x407F
#endif

struct metric_val {
  enum metric_val_type {
    INVALID,
    BOOL,
    FLOAT,
    UINT64,
    STRING,
  } type = INVALID;
  struct mval {
    std::variant<bool,float,uint64_t,std::string> value;

    mval() { }
    mval(bool x) : value(x) { }
    mval(float x) : value(x) { }
    mval(uint64_t x) : value(x) { }
    mval(const char *x) : value(x) { }
    operator bool() const {return std::get<bool>(value);}
    operator float() const {return std::get<float>(value);}
    operator uint64_t() const {return std::get<uint64_t>(value);}
    operator std::string() const {return std::get<std::string>(value);}
  };
  std::string metric;
  std::string units;
  bool is_info = false;
  std::vector<mval> values;
};
// linear lookup of metric columns based on name
struct metric_map {
  std::vector<metric_val> columns;

  void append(const char *metric, const char *units, bool is_info, bool x)
  {
    append_val(metric, units, is_info, metric_val::BOOL, x);
  }
  void append(const char *metric, const char *units, bool is_info, uint32_t x)
  {
    append_val(metric, units, is_info, metric_val::UINT64, (uint64_t)x);
  }
  void append(const char *metric, const char *units, bool is_info, uint64_t x)
  {
    append_val(metric, units, is_info, metric_val::UINT64, x);
  }
  void append(const char *metric, const char *units, bool is_info, float x)
  {
    append_val(metric, units, is_info, metric_val::FLOAT, x);
  }
  void
  append(const char *metric, const char *units, bool is_info, const char *x)
  {
    append_val(metric, units, is_info, metric_val::STRING, x);
  }

private:
  void append_val(
      const char *metric,
      const char *units,
      bool is_info,
      metric_val::metric_val_type ty,
      metric_val::mval v);
}; // metric_map

struct ms_info {
  std::string group;
  std::string set;
  std::vector<std::string> metrics;

  ms_info(std::string g, std::string s) : group(g), set(s) { }
};

// STATES:
// - [unbound]: after construction
//    - can only call is_loaded(), list_metric_sets(), ...
// - [bound]: to a metric set
//    - call bind(..) to get here from [unbound] or [bound]
//    - can call get_configuration()
//    - may not call if [active]
// - [active]: activate requires bind to be called first
//    - decativeate goes from [active] to in [bound]
struct mdapi_lib {
  void *impl;

  mdapi_lib();
  mdapi_lib(const mdapi_lib &) = delete;
  mdapi_lib & operator=(const mdapi_lib &) = delete;
  ~mdapi_lib();

  // queries if the library loaded and all entry points are setup correctly
  bool is_loaded() const;

  // fetches the an extended error string
  std::string get_error() const;

  // returns the version codes
  std::string get_version() const;

  // lists all metric sets
  std::vector<ms_info> list_metric_sets() const;

  // sets the current metric set
  bool bind(const char *metric_set_name);

  // call only after bind
  uint32_t get_configuration() const;
  //
  // size in bytes X needed by
  // clGetEventProfilingInfo(e, CL_PROFILING_COMMAND_PERFCOUNTERS_INTEL, X ,...)
  uint32_t get_query_report_size() const;

  bool activate();
  bool deactivate();

  bool parse_counter_buffer(metric_map &m, void *buf);
};

void list_mdapi_metrics(int verbosity, std::vector<std::string> ms_filts);

#endif // MDAPI_WRAPPER_HPP