#pragma once

#include <cmath>
#include <algorithm>
#include <vector>
#include <limits>

class stream_sampler {
  unsigned long long m_samples;
  double m, s;
  double m_min, m_max, m_sum;

public:
  stream_sampler() {
    reset();
  }

  void reset() {
    m_samples = 0;
    s = m = 0;
    m_min = m_max = 0;
    m_sum = 0;
  }

  void add(double x) {
    // Knuth TAOCP 2, sec 4.2.2.
    if (m_samples == 0) {
      m = x;
      s = 0.0;
      m_min = m_max = x;
    } else {
      double new_mean = m + (x - m) / (m_samples + 1);
      s = s + (x - m) * (x - new_mean);
      m = new_mean;
      if (x > m_max) m_max = x;
      else if (x < m_min) m_min = x;
    }
    m_sum += x;
    m_samples++;
  }

  unsigned long long size() const { return m_samples; }
  double sum() const { return m_sum; }
  double avg() const {
    return m_samples == 0 ? std::numeric_limits<double>().quiet_NaN() : m;
  }
  double var() const { return m_samples == 0 ? 0.0 : s / m_samples; } // biased
  double stdev() const { return sqrt(var()); }
  double sterr() const {
     return m_samples < 1 ? 0 : stdev() / sqrt((double)m_samples);
  }
  double cfv() const { return m_samples < 1 ? 0 : stdev() / avg(); }
  double min() const { return m_min; }
  double max() const { return m_max; }
}; // stream_sampler

class sampler : public stream_sampler {
  std::vector<double> m_samples;
public:
  sampler() { }

  void reset() {
    stream_sampler::reset();
  }
  void add(double x) {
    stream_sampler::add(x);
    m_samples.push_back(x);
  }
  const std::vector<double>& samples() const {
    return m_samples;
  }
  double med() const {
    if (m_samples.empty()) {
      return std::numeric_limits<double>().quiet_NaN();
    }
    std::vector<double> copy = m_samples;
    std::nth_element(copy.begin(), copy.begin() + copy.size()/2, copy.end());
    return copy[copy.size()/2];
  }
  // other methods that leverage all samples
};
