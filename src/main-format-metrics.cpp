#include <fstream>
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>

#include "../deps/mdapi/mdapi_wrapper.hpp"
#include "fatal.hpp"
#include "ir/cls_ir.hpp"
#include "processors/cls_interp.hpp"

static std::string format_metric_value(const metric_val::mval &mv) {
  std::stringstream ss;
  if (std::holds_alternative<bool>(mv.value)) {
    ss << ((bool)mv ? "T" : "F");
  } else if (std::holds_alternative<float>(mv.value)) {
    ss << std::fixed << std::setprecision(3) << (float)mv;
  } else if (std::holds_alternative<uint64_t>(mv.value)) {
    ss << (uint64_t)mv;
  } else if (std::holds_alternative<std::string>(mv.value)) {
    ss << (std::string)mv;
  } else {
    ss << "?";
  }
  return ss.str();
}


static void emit_metrics_nat(const cls::opts &os, const cls::mdapi_ctrs &mdcs) {
  for (const auto &m : mdcs) {
    const cls::dispatch_spec *ds = std::get<0>(m);
    std::cout << "==============";
    ds->str(std::cout, cls::format_opts());
    std::cout << "\n";
    const metric_map &mm = std::get<1>(m);
    unsigned max_col_len = 0;
    for (const metric_val &col : mm.columns) {
      if (col.is_info && os.verbosity <= 1)
        continue;
      std::cout << "  " << std::setw(28) << col.metric;
      max_col_len = std::max(max_col_len, (unsigned)col.values.size());
    }
    std::cout << "\n";
    for (unsigned row_ix = 0; row_ix < max_col_len; row_ix++) {
      std::cout << "  ";
      for (const metric_val &col : mm.columns) {
        if (col.is_info && os.verbosity <= 1)
          continue;
        std::cout << "  ";
        std::stringstream ss;
        if (row_ix >= (unsigned)col.values.size()) {
          ss << "-";
        } else {
          const metric_val::mval &mv = col.values[row_ix];
          ss << format_metric_value(mv);
        }
        std::cout << std::setw(28) << ss.str();
      }
      std::cout << "\n";
    }
  }
}

static void
emit_metrics_trns(const cls::opts &os, const cls::mdapi_ctrs &mdcs)
{
  for (const auto &m : mdcs) {
    const cls::dispatch_spec *ds = std::get<0>(m);
    std::cout << "==============";
    ds->str(std::cout, cls::format_opts());
    std::cout << "\n";
    const metric_map &mm = std::get<1>(m);
    unsigned max_metric_col = 0, max_units_col = 0;
    for (const metric_val &col : mm.columns) {
      max_metric_col = std::max(max_metric_col, (unsigned)col.metric.size());
      max_units_col = std::max(max_units_col, (unsigned)col.units.size());
    }

    for (const metric_val &col : mm.columns) {
      if (col.is_info && os.verbosity <= 1)
        continue;
      std::cout << "  " << std::setw(max_metric_col) << std::left << col.metric;
      std::cout << "  " << std::setw(max_units_col) << std::left << col.units;
      for (const auto &mv : col.values) {
        std::cout << "  " << std::setw(16) << std::right
                  << format_metric_value(mv);
      }
      std::cout << "\n";
    }
  }
}

static void emit_metrics_csv(const cls::opts &os, const cls::mdapi_ctrs &mdcs)
{
  const char *OUTPUT_CSV_FILE = "metrics.csv";
  std::ofstream ofs {OUTPUT_CSV_FILE, std::ofstream::out};
  if (!ofs.good()) {
    std::cerr << OUTPUT_CSV_FILE << ": failed to open file\n";
    exit(EXIT_FAILURE);
  }
  for (const auto &m : mdcs) {
    const cls::dispatch_spec *ds = std::get<0>(m);
    ofs << "\"";
    ds->str(ofs, cls::format_opts());
    ofs << "\"";
    ofs << "\n";
    const metric_map &mm = std::get<1>(m);
    unsigned max_col_len = 0;
    for (const metric_val &col : mm.columns) {
      if (col.is_info && os.verbosity <= 1)
        continue;
      ofs << "," << col.metric;
      max_col_len = std::max(max_col_len, (unsigned)col.values.size());
    }
    ofs << "\n";
    for (unsigned row_ix = 0; row_ix < max_col_len; row_ix++) {
      for (const metric_val &col : mm.columns) {
        if (col.is_info && os.verbosity <= 1)
          continue;
        std::stringstream ss;
        if (row_ix < (unsigned)col.values.size()) {
          const metric_val::mval &mv = col.values[row_ix];
          ss << format_metric_value(mv);
        }
        ofs << "," << ss.str();
      }
      ofs << "\n";
    } // cols
  }
  if (os.verbosity >= 0) {
    std::cout << OUTPUT_CSV_FILE << ": wrote data to file\n";
  }
}

// static void emit_metrics_trns_csv(const cls::opts &os, const cls::mdapi_ctrs &mdcs);

void emit_metrics(const cls::opts &os, const cls::mdapi_ctrs &mdcs)
{
  if (os.metric_format == cls::opts::NAT) {
    emit_metrics(os, mdcs);
  } else if (os.metric_format == cls::opts::TRANS) {
    emit_metrics_trns(os, mdcs);
  } else if (os.metric_format == cls::opts::CSV) {
    emit_metrics_csv(os, mdcs);
  } else {
    std::cerr << "unexpected output format for -mf=..\n";
    exit(EXIT_INTERNAL_ERROR);
  }
}
