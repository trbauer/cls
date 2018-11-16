#ifndef LIST_MAP
#define LIST_MAP

#include <iterator>
#include <unordered_map>
#include <vector>

// Generates an object that can be used in a for loop
// It wraps iterator type I and maps values with function F
// TYPE PARAMS:
//    V = the value type returned by the iterator on dereference (V&)
//    F = the function we wish to apply to the iterator contents before
//        returning the dereferenced; this must map the given iterator type's
//        return value to V.
//    I = the underlying iterator type we are mapping
template <typename V, typename F, typename I>
struct mapping_iterator {
  I itr;
  explicit mapping_iterator(I &beg) : itr(beg) { }
  I& operator++() {itr++; return *this;}
  I& operator++(int) {I ret = *this; ++(*this); return ret;}
  bool operator==(I other) const {return itr == other.itr;}
  bool operator!=(I other) const {return !(*this == other);}
  V& operator*() const {return F(itr->second);}
};

template <typename V>
struct identity_mapping {
  const V& operator()(const V &t) const {return v;}
        V& operator()(V &t) {return v;}
};
template <typename P,typename V>
struct pair_mapping {
        V& operator()(P &p) {return p.second;}
  const V& operator()(const P &p) const {return p.second;}
};

template <typename V, typename F, typename I>
struct mapped_for {
  using iterator = mapping_iterator<V,F,I>;
  using const_iterator = mapping_iterator<const V,F,I>;
  const iterator beg_itr, end_itr;
  explicit mapped_for(I &_beg,I &_end) : beg_itr(_beg), end_itr(_end) { }
  const_iterator  begin()  const {return beg_itr;}
  const_iterator  end()    const {return end_itr;}
  iterator        begin()        {return beg_itr;}
  iterator        end()          {return end_itr;}
};


// A map that iterates in list order
//
// One can iterate the key value pairs in order or just the values.
//     list_map<key,foo>  map;
//     for (std::pair<key,foo> &p : map)
template<typename K,typename V>
struct list_map {
  using lm_list = typename std::vector<std::pair<K,V>>;
  using lm_map = typename std::unordered_map<K,V*>;
  using pair_iterator = typename std::vector<std::pair<K,V>>::iterator;
  using const_pair_iterator = typename std::vector<std::pair<K,V>>::const_iterator;
  /*
  using value_iterator =
    mapping_iterator<V,pair_mapping<std::pair<K,V>,V>,lm_list::iterator>;
  using const_value_iterator =
    mapping_iterator<
      const V,pair_mapping<const std::pair<K,V>,const V>,
      lm_list::const_iterator>;
  using pair_iterator =
    mapping_iterator<V,identity_mapping<std::pair<K,V>>,lm_list::iterator>;
*/
/*
        value_iterator begin()       {return mapping_iterator(list.begin());}
        value_iterator end()         {return mapping_iterator(list.end());}
  const value_iterator begin() const {return list.begin();}
  const value_iterator end()   const {return list.end();}
  */
  using mapped_pairs_for = mapped_for<
                            std::pair<K,V>,
                            identity_mapping<std::pair<K,V>>,
                            pair_iterator>;
  using mapped_pairs_for_const = mapped_for<
                            const std::pair<K,V>,
                            identity_mapping<const std::pair<K,V>>,
                            pair_iterator>;

  mapped_pairs_for_const pairs() const {
    return mapped_pairs_for(list.begin(),list.end());
  }
  mapped_pairs_for pairs() {
    return mapped_pairs_for(list.begin(),list.end());
  }

  pair_iterator pair_begin()             {return list.begin();}
  pair_iterator pair_end()               {return list.end();}
  const_pair_iterator pair_begin() const {return list.begin();}
  const_pair_iterator pair_end()   const {return list.end();}

  // omap::const_miterator find(const K &k) const {return map.find(k);}
  //      omap::miterator find(const K &k) {return map.find(k);}

  template <typename...As>
  V &get(const K &k,As...as) {
    auto itr = map.find(k);
    if (itr == map.end()) {
      return *itr->second;
    }
    // C++17 V& obj = list.emplace_back(std::make_tuple(V,as...));
    list.emplace_back(std::make_tuple(k,as...));
    std::pair<K,V> &p = list.back();
    V& obj = p.second;
//    map.emplace_hint(itr, k, &obj);
    return obj;
  }
private:
  lm_list       list;
  lm_map        map;
};

#endif
