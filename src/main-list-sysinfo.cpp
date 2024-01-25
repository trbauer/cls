#include "cls_opts.hpp"
#include "system.hpp"
#include "text.hpp"
#ifdef WIN32
#include <Windows.h>
#endif // WIN32
#include <initializer_list>

using namespace text;



static std::string expand_bitfield(
    uint64_t bits, std::initializer_list<std::pair<const char *, uint64_t>> ps)
{
  std::stringstream ss;
  bool first = true;
  for (auto [sym, smask] : ps) {
    if (bits & smask) {
      bits &= ~smask;
      if (first) {
        first = false;
      } else {
        ss << "|";
      }
      ss << sym;
    }
  }
  if (bits) {
    if (!first) {
      ss << "|";
    }
    ss << "0x" << hex(bits);
  } else if (first) {
    ss << "0x0";
  }
  return ss.str();
}

#ifdef WIN32
static BOOL list_monitor(
  HMONITOR unnamedParam1,
  HDC,
  LPRECT,
  LPARAM unnamedParam4)
{
  static_assert(sizeof(unnamedParam4) == sizeof(void*));
  std::vector<std::string> *pinfo = (std::vector<std::string> *)unnamedParam4;
  MONITORINFOEXA mi {0};
  mi.cbSize = sizeof(mi);
  if (!GetMonitorInfoA(unnamedParam1, &mi)) {
    std::cerr << "GetMonitorInfo[" << pinfo->size() << "]: "
              << "failed " << GetLastError() << "\n";
  } else {
    std::stringstream ss;
    ss << "GetMonitorInfo[" << pinfo->size() << "]: " << mi.szDevice << " ";
    auto rect_str = [&](const char *key, RECT r) {
      ss << key << ":"
         << "RECT{",
          r.left, " ", r.top, " ", r.right, " ", r.bottom, "}";
    };
    rect_str("rcMonitor", mi.rcMonitor);
    ss << ", ";
    rect_str("rcWork", mi.rcWork);
    ss << ", dwFlags:";
    ss << expand_bitfield(
              mi.dwFlags,
              {{"MONITORINFOF_PRIMARY", MONITORINFOF_PRIMARY}});
    ss << "\n";
    pinfo->push_back(ss.str());
  }
  return TRUE; // continue enumeration
}

static void list_system_info_win32(const cls::opts &opts) {
  std::ostream &os = std::cout;

  os << "============== DISPLAY DEVICES\n";
  DISPLAY_DEVICEA dd = { 0 };
  dd.cb = sizeof(dd);
  for (DWORD ix = 0; EnumDisplayDevicesA(NULL, ix, &dd, 0); ix++) {
    os << "EnumDisplayDevicesA(...," << ix << ", ...)\n";
    os << "  DeviceName:    " << dd.DeviceName << "\n";
    os << "  DeviceString:  " << dd.DeviceString << "\n";
    os << "  DeviceID:      " << dd.DeviceID << "\n";
    os << "  DeviceKey:     " << dd.DeviceKey << "\n";
    os << "  StateFlags:    ";
    os << expand_bitfield(
              dd.StateFlags,
              {{"DISPLAY_DEVICE_ACTIVE", DISPLAY_DEVICE_ACTIVE},
               {"DISPLAY_DEVICE_MIRRORING_DRIVER",
                DISPLAY_DEVICE_MIRRORING_DRIVER},
               {"DISPLAY_DEVICE_MODESPRUNED", DISPLAY_DEVICE_MODESPRUNED},
               {"DISPLAY_DEVICE_PRIMARY_DEVICE", DISPLAY_DEVICE_PRIMARY_DEVICE},
               {"DISPLAY_DEVICE_REMOVABLE", DISPLAY_DEVICE_REMOVABLE},
               {"DISPLAY_DEVICE_VGA_COMPATIBLE",
                DISPLAY_DEVICE_VGA_COMPATIBLE}})
       << "\n";
  }

  os << "============== MONITORS\n";
  std::vector<std::string> info;
  if (!EnumDisplayMonitors(NULL, NULL, &list_monitor, (LPARAM)&info)) {
    std::cerr << "EnumDisplayMonitors: failed: " << GetLastError() << "\n";
  }
}

#else // !WIN32

static void list_system_info_linux(const cls::opts &opts) {
  std::ostream &os = std::cout;
  os << "not implemented for Linux\n";
}
#endif // !WIN32


void list_system_info(const cls::opts &os)
{
#ifdef WIN32
  list_system_info_win32(os);
#else // !WIN32
  list_system_info_linux(os);
#endif // !WIN32
}
