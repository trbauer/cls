#ifndef _OPTS_HPP_
#define _OPTS_HPP_

#include "system.hpp"

#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>


namespace opts
{
  static void fatal_invalid_spec(std::string what) {
    std::cerr <<
      "INTERNAL ERROR: invalid CmdlineSpec (opts.hpp):\n" << what << "\n";
    sys::fatal_exit();
  }

  static bool streq(const char *str1, const char *str2)
  {
    return str1 == str2 || (str1 && str2 && strcmp(str1, str2) == 0);
  }
  static bool strpfx(const char *pfx, const char *str)
  {
    return strncmp(str, pfx, strlen(pfx)) == 0;
  }
  static bool readDecInt(const char *str, int &val)
  {
    char *end = nullptr;
    val       = (int)strtol(str, &end, 10);
    if (end != str + strlen(str)) {
      return false;
    }
    return true;
  }

  // takes message and handles the error; second arg is an optional usage
  // message typedef std::function<void(const char *, const char *)>
  // error_handler_t;
  struct ErrorHandler {
    const char *exeName;

    ErrorHandler(const char *exe) : exeName(exe) {}

    void operator()(const std::string &msg) const
    {
      std::cerr << msg << "\n";
      sys::fatal_exit();
    }
    void operator()(const std::string &msg, const std::string &usage) const
    {
      std::cerr << msg << "\n";
      std::cerr << usage;
      sys::fatal_exit();
    }
  };

  static std::string concat(
      const std::string &a,
      const std::string &b,
      const std::string &c = "",
      const std::string &d = "",
      const std::string &e = "",
      const std::string &f = "")
  {
    std::stringstream ss;
    ss << a << b << c << d << e << f;
    return ss.str();
  }

  // sets a value in the options set
  template<typename O>
  using Setter = std::function<void(const char *, const ErrorHandler &, O &)>;

  // sets option or arg to it's default value
  template<typename O>
  using DefaultSetter = std::function<void(const ErrorHandler &, O &)>;

  enum OptAttrs {
    NONE        = 0x00,
    ALLOW_MULTI = 0x01, // option can be specified multiple times
    REQUIRED    = 0x02, // not an error if the option is never set
    FLAG        = 0x04, // option takes no argument (setValue passed nullptr)
    FLAG_VALUE  = 0x08, // optional flag value: for stuff like the -h option
                        // which can be -h or -h=foo
    FUSED_VALUE = 0x10, // the option value is fused to the name
                        // i.e this option permits matching an option
                        // key/values without the = character between name
                        // and value (e.g. -DFOO=BAR means -D=FOO=BAR).
                        // The latter will be permitted too.

    // HIDDEN = 0x20    // for a hidden option (hidden from help message)
  };

  static std::string fmt_opt_key(
    bool isLongKey, const char *key, OptAttrs attrs)
  {
    bool isFlag = attrs & OptAttrs::FLAG,
      isFused = attrs & OptAttrs::FUSED_VALUE;
    if (key == nullptr)
      return "";
    std::stringstream ss;
    ss << '-';
    if (isLongKey)
      ss << '-';
    ss << key;
    if (!isFused)
      ss << "=";
    if (!isFlag) {
      ss << "..";
    }
    return ss.str();
  }

  // An option specification specifies all the info about an option
  // It has lambda functions for parsing and setting the value as well as
  // specifying a default value.
  //
  // Type parameter is for the actual options structure.
  template<typename O>
  struct Opt {
    const char *groupPrefix;
    const char *shortName;
    const char *longName;
    const char *typeName;
    const char *description;
    const char *extendedDescription;
    const char *group;

    Setter<O>        setValue;   // called to set a value
    DefaultSetter<O> setDefault; // called when a option is unspecified

    int timesMatched;

    OptAttrs attributes;

    // constructor option specification with default
    Opt(const char *     g,
        const char *     s,
        const char *     l,
        const char *     t,
        const char *     d,
        const char *     xd,
        int              attrs, // OptAttrs
        Setter<O>        setVal,
        DefaultSetter<O> setDflt)
        : groupPrefix(g),
          shortName(s),
          longName(l),
          typeName(t),
          description(d),
          extendedDescription(xd),
          attributes((OptAttrs)attrs),
          setValue(setVal),
          setDefault(setDflt),
          timesMatched(0)
    {
    }

    // constructor for option specification without default
    Opt(const char *g,
        const char *s,
        const char *l,
        const char *t,
        const char *d,
        const char *xd,
        int         attrs, // OptAttrs
        Setter<O>   setVal)
        : groupPrefix(g),
          shortName(s),
          longName(l),
          typeName(t),
          description(d),
          extendedDescription(xd),
          setValue(setVal),
          setDefault(noDefault()),
          timesMatched(0),
          attributes((OptAttrs)attrs)
    {
    }

    std::string fmtShortNameForHelp() const {
      return fmt_opt_key(false,
        o.shortName,
        attrs & OptAttrs::FLAG,
        OptAttrs::FUSED_VALUE);
    }
    std::string fmtLongNameForHelp() const {
      return fmt_opt_key(true, o.longName,
        attrs & OptAttrs::FLAG,
        attrs & OptAttrs::FUSED_VALUE);
    }

    DefaultSetter<O> noDefault() const {
      std::string optStr = optName(), helpStr = makeHelpMessage();
      return [=](const ErrorHandler &errHandler, O &) {
        errHandler(concat(optStr, " undefined"), helpStr);
      };
    }

    bool hasAttribute(enum OptAttrs attr) const {
      return (attributes & attr) != 0;
    }

    // options have a non-null short or long name
    bool isArg() const { return !shortName && !longName; }
    // arguments have both null long and short names
    bool isOpt() const { return !isArg(); }

    bool tryMatch(
      int                 argc,
      const char **       argv,
      int &               argIx,
      const ErrorHandler &errHandler,
      O &                 opts)
    {
      bool        optCheck        = isOpt();
      std::string msgHelp         = makeHelpMessage();
      auto        raiseMatchError = [&](const char *msg) {
        errHandler(
          concat(
            argv[argIx],
            ": invalid ",
            optCheck ? "option" : "argument",
            ": ",
            msg,
            "\n"),
          msgHelp);
      };
      const char *token = argv[argIx];
      const char *value = nullptr;

      if (isOpt()) {
        if (token[0] != '-')
          return false;

        // group prefix; e.g. -Xfoo
        int off = 1;
        if (groupPrefix) {
          if (!strpfx(groupPrefix, token + 1)) {
            return false;
          }
          off += 1;
        }

        const char *key;
        if (shortName && token[0] == '-' && strpfx(shortName, token + off)) {
          // short option -f=... or -f ...
          key   = token + off;
          value = key + strlen(shortName);
        } else if (
          longName && token[0] == '-' && token[1] == '-' &&
          strpfx(longName, token + off + 1))
        {
          // long option --foo=... or --foo ...
          key   = token + off + 1;
          value = key + strlen(longName);
        } else {
          // no match
          return false;
        }

        if (*value == 0) {
          // -flag
          // OR
          // -key value
          if (hasAttribute(FLAG) || hasAttribute(opts::FLAG_VALUE)) {
            // -flag
            value = "";
          } else if (argIx == argc - 1) {
            raiseMatchError("unexpected end of command line");
          } else {
            // not a flag and we have tokens left
            // next token is the value for this option
            argIx++;
            value = argv[argIx];
          }
        } else if (*value == '=') {
          // of the form -key=value
          if (hasAttribute(FLAG) && !hasAttribute(FLAG_VALUE)) {
            raiseMatchError("option is a flag");
          } else {
            value++; // step past =
          }
        } else {
          // junk at end of key (recall, we strncmp'd)
          // e.g. this option is "--foo" and the token is "--food"
          // this is just a mismatch (no harm done)
          return false;
        }

        // ensure the option hasn't been specified before
        if (timesMatched > 0 && !hasAttribute(OptAttrs::ALLOW_MULTI)) {
          raiseMatchError("respecification");
        }
      } else { // isArg()
        if (timesMatched > 0 && !hasAttribute(OptAttrs::ALLOW_MULTI)) {
          // arg that may only be specified once, skip it (there might
          // be another arg, so it's not an error)
          return false;
        }
        value = token;
      } // end isOpt() / isArg()

      // attempt to parse the input
      setValue(value, errHandler, opts);
      timesMatched++;

      // move past key or value to next token
      argIx++;
      return true;
    }

    void appendHelpMessage(
        std::ostream &os,
        int           sCw           = 0,
        int           lCw           = 0,
        int           tCw           = 0,
        bool          appendExtDesc = true) const
    {
      // if column width is unspecified, then rescale it based on the
      // actual value's lengths
      const char *groupName     = groupPrefix ? groupPrefix : "";
      int         grLen         = (int)strlen(groupName);
      auto autoScaleColumnWidth = [&](int &cw, int baseCw, const char *colStr) {
        if (colStr)
          cw = std::max(cw, std::max(baseCw, (int)strlen(colStr) + grLen));
        else
          cw = baseCw;
      };
      autoScaleColumnWidth(sCw, 4, shortName);
      autoScaleColumnWidth(lCw, 2, longName);
      autoScaleColumnWidth(tCw, 8, typeName);

      if (isOpt()) {
        if (shortName) {
          os << std::setw(3 + sCw) << std::left
             << concat("  -", groupName, shortName);
        } else {
          os << std::setw(3 + sCw) << "";
        }
        os << "  ";
        if (longName) {
          os << std::setw(4 + lCw) << std::left
             << concat("  --", groupName, longName);
        } else {
          os << std::setw(4 + lCw) << "";
        }
      }
      std::string typeNameStr = typeName ? typeName : "";
      if (isArg() && hasAttribute(ALLOW_MULTI))
        typeNameStr += (hasAttribute(REQUIRED) ? '+' : '*');
      else
        typeNameStr += ' ';
      os << "  " << std::setw(tCw) << typeNameStr;
      if (description)
        os << "  " << description;
      if (appendExtDesc && extendedDescription) {
        os << "\n";
        // auto format the description (crudely for now)
        size_t col = 1, slen = strlen(extendedDescription);
        for (size_t i = 0; i < slen; i++, col++) {
          char c = extendedDescription[i];
          if (col > 72 && c == ' ') {
            col = 0;
            os << "\n";
            while (i + 1 < slen && isspace(extendedDescription[i + 1]))
              i++;
            // i++ at loop end gets the last space
          } else {
            os << c;
          }
        }
      }
    }

    // convenience accessor (for errors)
    std::string makeHelpMessage(
        int sCw = 0, int lCw = 0, int tCw = 0, bool appendExtDesc = true) const
    {
      std::stringstream ss;
      appendHelpMessage(ss, sCw, lCw, tCw, appendExtDesc);
      return ss.str();
    }

    std::string optName() const
    {
      std::string str;
      if (isArg()) {
        str += "argument ";
      } else {
        str += "option ";
      }
      if (shortName) {
        str += "-";
        str += shortName;
      } else if (longName) {
        str = "--";
        str += longName;
      }
      return str;
    }
  };

  // Specifies a group of options all starting with the same prefix
  // E.g. experimental options might start with "X", compiler warnings
  // might be enabled or disabled with -Winexact-values syntax
  template<typename O>
  struct Group {
    const char *        prefix; // e.g. "X"
    const char *        name;   // e.g. "Experimental Options"
    std::vector<Opt<O>> members;

    Group(const char *prefix_, const char *name_)
      : prefix(prefix_), name(name_) { }
    Group(Group &&copy)
    {
      prefix  = copy.prefix;
      name    = copy.name;
      members = std::move(copy.members);
    }
    Group(const Group &copy) = delete;
    Group operator=(const Group &) = delete;

    /////////////////////////////////////////////////////////////////////////////
    // FLAGS
    //
    // a flag requires no argument, but allows for one
    void defineFlag(
        const char *sNm,
        const char *lNm,
        const char *desc,
        const char *extDesc,
        int         attrs, // OptAttrs
        bool&       boolValue)
    {
      defineFlag(
          sNm,
          lNm,
          desc,
          extDesc,
          attrs | FLAG,
          [&boolValue](const char *value, const opts::ErrorHandler &, O &) {
            if (value && streq(value, "false"))
              boolValue = false;
            else
              boolValue = true;
          });
    }
    void defineFlag(
        const char *sNm,
        const char *lNm,
        const char *desc,
        const char *extDesc,
        int         attrs, // OptAttrs
        Setter<O>   setVal)
    {
      ensureUnique(sNm, lNm, (OptAttrs)attrs);
      Opt<O> temp(prefix, sNm, lNm, "", desc, extDesc, attrs | FLAG, setVal);
      members.emplace_back(temp);
    }
    /////////////////////////////////////////////////////////////////////////////
    // OPTIONS
    void defineOpt(
        const char *sNm,
        const char *lNm,
        const char *type,
        const char *desc,
        const char *extDesc,
        int         attrs, // OptAttrs
        int&        val)
    {
      defineOpt(
          sNm,
          lNm,
          type,
          desc,
          extDesc,
          attrs,
          [&] (const char *value, const opts::ErrorHandler &eh, O &)
          {
            if (!readDecInt(value, val)) {
              std::stringstream ss;
              if (sNm)
                ss << "-" << sNm;
              else
                ss << "--" << lNm;
              ss << ": malformed argument (integer)";
              eh(ss.str().c_str());
            }
          });
    }
    void defineOpt(
        const char * sNm,
        const char * lNm,
        const char * type,
        const char * desc,
        const char * extDesc,
        int          attrs, // OptAttrs
        std::string &stringValue)
    {
      defineOpt(
          sNm,
          lNm,
          type,
          desc,
          extDesc,
          attrs,
          [&](const char *value, const opts::ErrorHandler &, cls::opts &) {
            stringValue = value;
          });
    }
    // generic version that takes a lambda
    void defineOpt(
        const char *sNm,
        const char *lNm,
        const char *type,
        const char *desc,
        const char *extDesc,
        int         attrs, // OptAttrs
        Setter<O>   setVal)
    {
      ensureUnique(sNm, lNm, (OptAttrs)attrs);
      Opt<O> temp(prefix, sNm, lNm, type, desc, extDesc, attrs, setVal);
      members.emplace_back(temp);
    }

    // a default setter sets the option to something after everything else is
    // parsed (and this one doesn't get an explicit setting)
    void defineOpt(
        const char *     sNm,
        const char *     lNm,
        const char *     type,
        const char *     desc,
        const char *     extDesc,
        int              attrs, // OptAttrs
        Setter<O>        setVal,
        DefaultSetter<O> setDftVal)
    {
      ensureUnique(sNm, lNm, (OptAttrs)attrs);
      Opt<O> temp(
          prefix, sNm, lNm, type, desc, extDesc, attrs, setVal, setDftVal);
      members.emplace_back(temp);
    }

    /////////////////////////////////////////////////////////////////////////////
    bool tryMatch(
        int                 argc,
        const char **       argv,
        int &               argIx,
        const ErrorHandler &errHandler,
        O &                 opts)
    {
      for (auto &o : members) {
        if (o.tryMatch(argc, argv, argIx, errHandler, opts)) {
          return true;
        }
      }
      return false;
    }

    void ensureUnique(const char *sNm, const char *lNm, OptAttrs attrs) {
      ensureUniqueKey(false, sNm, attrs);
      ensureUniqueKey(true,  lNm, attrs);
    }

    void ensureUniqueKey(bool isLongKey, const char *key, OptAttrs attrs)
    {
#ifdef _DEBUG
      if (key == nullptr)
        return;
      // ensure they didn't prefix their own -'s
      if (*key == '-') {
        fatal_invalid_spec(concat("opt: ",
          key, ": name should not start with (the - or -- is implicit)"));
      }
      bool fused = (attrs & OptAttrs::FUSED_VALUE) != 0;

      auto nameCollides = [&](
        const char *oKey, bool oFused)
      {
        if (oKey == nullptr)
          return false;
        // CASES:
        //  fused  oFused
        //  F      F      -A=..  -B=..   e.g. -D=      and -D=
        //  F      T      -A=..  -B..    e.g. -DOG=..  and -D..
        //  T      F      -A..   -B=..   e.g. -D..     and -DOG=..
        //  T      T      -A..   -B..    e.g. -D..     and -DOG.. or
        //                                    -DOG..   and -D..
        if (!fused && !oFused) {
          return streq(key, oKey);
        } else if (!fused) { // && oFused
          return strpfx(oKey, key);
        } else if (!oFused) { // && fused
          return strpfx(key, oKey);
        } else {
          return strpfx(key, oKey) || strpfx(oKey, key);
        }
      };
      auto checkName = [&](bool oIsLong, const char *oKey, OptAttrs oAttrs) {
        bool oFused =  oAttrs & OptAttrs::FUSED_VALUE;
        if (nameCollides(oKey, oFused)) {
          std::stringstream ss;
          ss << "ambiguous specification; "
            "two or more options match the same inputs\n";
          ss << "  "  << fmt_opt_key(oIsLong, oKey, oAttrs) << "\n";
          ss << "and\n";
          ss << "  "  << fmt_opt_key(isLongKey, key, attrs) << "\n";
          fatal_invalid_spec(ss.str());
        }
      };

      // ensure doesn't collide with main name
      for (const auto &o : members) {
        checkName(false, o.shortName, o.attributes);
        checkName(true, o.longName, o.attributes);
      }
#endif
    }

    void appendGroupSummary(std::ostream &os) const
    {
      if (prefix) {
        os << "OPTION GROUP " << prefix << "\n";
      }
      int sCw = 4, lCw = 8, tCw = 8;
      for (auto &o : members) {
        auto updateMax = [&](int cw, const char *str) {
          return str ? std::max(cw, (int)strlen(str)) : cw;
        };
        sCw = updateMax(sCw, o.shortName);
        lCw = updateMax(lCw, o.longName);
        tCw = updateMax(tCw, o.typeName);
      }
      for (auto &o : members) {
        o.appendHelpMessage(os, sCw, lCw, tCw, false);
        os << "\n";
      }
    }
  };

  struct ExtraHelp {
    std::string key;
    std::string shortDesc;
    std::string longDesc;
    ExtraHelp(std::string k, std::string s, std::string l)
      : key(k), shortDesc(s), longDesc(l) { }
  };

  template<typename O>
  class CmdlineSpec {
    const char *            exeTitle;
    const char *            exeName;
    Group<O>                opts;      // top-level command line options
    std::vector<Group<O> *> groups;    // special option groups (e.g. -X....)
    std::vector<Opt<O>>     args;      // command line arguments
    const char *            examples;
    std::vector<ExtraHelp>  extraHelp;

  public:
    CmdlineSpec(
        const char *title,
        const char *exe,
        const char *examps        = "",
        bool        appendHelpOpt = true)
        : exeTitle(title),
          exeName(exe),
          opts(nullptr, nullptr),
          examples(examps)
    {
      if (appendHelpOpt) {
        defineFlag(
          "h",
          "help",
          "shows help on an option",
          "Without any argument -h will print general help on all "
          "options. "
          "However, if given an argument, -h will attempt to lookup that "
          "argument. "
          "Options are given without preceding - or --.  We check long "
          "option "
          "names first, "
          "short names second, and finally argument indices last.  "
          "Argument "
          "indices are "
          "specified as: \"#1\" for the first argument, \"#2\" for the "
          "second, etc.\n"
          "\n"
          "EXAMPLES:"
          "  -h=h  lists this message\n"
          "  -h=foo lists info on option or flag -foo or --foo\n"
          "  -h=#0 lists info on the first command line argument (if available)\n"
          "",
          OptAttrs::FLAG_VALUE,
          [&](const char *inp, const ErrorHandler &err, O &) {
            handleHelpArgument(inp, err);
          }); // end  defineFlag(...)
      }       // end if appendHelpOpt
    }         // end constructor

    ///////////////////////////////////////////////////////////////////////////
    // These just redirect to the root group's options
    void defineFlag(
        const char *sNm,
        const char *lNm,
        const char *desc,
        const char *extDesc,
        int         attrs, // OptAttrs
        Setter<O>   setter)
    {
      ensureUnique(sNm, lNm, attrs);
      opts.defineFlag(sNm, lNm, desc, extDesc, attrs, setter);
    }
    void defineFlag(
        const char *sNm,
        const char *lNm,
        const char *desc,
        const char *extDesc,
        int         attrs, // OptAttrs
        bool&       var)
    {
      ensureUnique(sNm, lNm, attrs);
      opts.defineFlag(sNm, lNm, desc, extDesc, attrs, var);
    }
    void defineOpt(
        const char *sNm,
        const char *lNm,
        const char *type,
        const char *desc,
        const char *extDesc,
        int         attrs, // OptAttrs
        Setter<O>   setter)
    {
      ensureUnique(sNm, lNm, attrs);
      opts.defineOpt(sNm, lNm, type, desc, extDesc, attrs, setter);
    }
    // have to expand these manually
    template<typename T>
    void defineOpt(
        const char *sNm,
        const char *lNm,
        const char *type,
        const char *desc,
        const char *extDesc,
        int         attrs, // OptAttrs
        T&          ref)
    {
      ensureUnique(sNm, lNm, attrs);
      opts.defineOpt(sNm, lNm, type, desc, extDesc, attrs, ref);
    }
    void defineOpt(
        const char *     sNm,
        const char *     lNm,
        const char *     type,
        const char *     desc,
        const char *     extDesc,
        int              attrs, // OptAttrs
        Setter<O>        setter,
        DefaultSetter<O> defaultSetter)
    {
      ensureUnique(sNm, lNm, attrs);
      opts.defineOpt(
          sNm, lNm, type, desc, extDesc, attrs, setter, defaultSetter);
    }

    ///////////////////////////////////////////////////////////////////////////
    // ARGUMENTS
    void defineArg(
        const char *              type,
        const char *              desc,
        const char *              extDesc,
        int                       attrs, // OptAttrs
        std::vector<std::string> &args)
    {
      defineArg(
          type,
          desc,
          extDesc,
          attrs,
          [&](const char *value, const opts::ErrorHandler &, O &) {
            args.emplace_back(value);
          });
    }
    void defineArg(
        const char *type,
        const char *desc,
        const char *extDesc,
        int         attrs, // OptAttrs
        Setter<O>   setter)
    {
      Opt<O> temp(
          nullptr, nullptr, nullptr, type, desc, extDesc, attrs, setter);
      args.emplace_back(temp);
    }

    void defineArg(
        const char *     type,
        const char *     desc,
        const char *     extDesc,
        int              attrs, // OptAttrs
        Setter<O>        setter,
        DefaultSetter<O> defaultSetter)
    {
      Opt<O> temp(
          nullptr,
          nullptr,
          nullptr,
          type,
          desc,
          extDesc,
          attrs,
          setter,
          defaultSetter);
      args.emplace_back(temp);
    }

    Group<O> &defineGroup(const char *prefix, const char *name) {
      ensureUniqueKey(false, prefix, OptAttrs::NONE);
      groups.emplace_back(new Group<O>(prefix, name));
      return *groups.back();
    }

    // extra help sections
    void defineExtraHelpSection(
      std::string key,
      std::string shortDesc,
      std::string prose)
    {
      ensureUniqueKey(false, key.c_str(), OptAttrs::NONE);
      extraHelp.emplace_back(key, shortDesc, prose);
    }

private:
    // ensure option or key names are unique at the root level
    void ensureUnique(const char *sNm, const char *lNm, int attrs) {
      ensureUniqueKey(false, sNm, attrs);
      ensureUniqueKey(true, lNm, attrs);
    }
    void ensureUniqueKey(bool isLongName, const char *newKey, int attrs) {
#ifdef _DEBUG
      if (newKey == nullptr)
        return;
      opts.ensureUniqueKey(isLongName, newKey, (OptAttrs)attrs);
      for (const auto &eh : extraHelp) {
        if (eh.key == newKey)
          fatal_invalid_spec("name conflicts with extra help section");
      }
#endif
    }

    void handleHelpArgument(const char *inp, const ErrorHandler &err) {
      if (!inp || !*inp) {
        // no input given e.g. -h
        if (exeTitle)
          std::cout << exeTitle << "\n\n";
        appendUsage(
          std::cout,
          sys::is_tty(std::cerr),
          exeName,
          examples);
        exit(EXIT_SUCCESS);
      } else {
        // given input: e.g. -h foo OR -h #1
        //
        // first try extra help sections
        for (const auto &eh : extraHelp) {
          if (inp == eh.key) {
            std::cout << eh.longDesc;
            exit(EXIT_SUCCESS);
          }
        }
        // then descend into option groups
        const Opt<O> *opt       = nullptr;
        auto          scanGroup = [&](const Group<O> &g, bool pfx) {
          int  off   = 0;
          auto match = pfx ? strpfx : streq;
          if (g.prefix) {
            if (streq(g.prefix, inp) == 0) {
              // exact group match; bail and deal with it below
              return;
            } else if (g.prefix && streq(g.prefix, inp)) {
              // group prefix e.g. "Xfoo" for group "X"
              off += (int)strlen(g.prefix);
            }
          }
          for (auto &o : g.members) {
            // try long names
            if (o.longName && match(inp + off, o.longName)) {
              opt = &o;
              break;
            }
            // try short names
            if (o.shortName && match(inp + off, o.shortName)) {
              opt = &o;
              break;
            }
          }
        };

        // try for exact option matches first (prefers exact over
        // partial matches)
        scanGroup(opts, false);
        if (!opt) {
          for (auto &g : groups) {
            scanGroup(*g, false);
          }
        }
        // try for an exact group match first
        // Group<O> *group = nullptr;
        for (auto g : groups) {
          if (streq(g->prefix, inp)) {
            g->appendGroupSummary(std::cout);
            exit(EXIT_SUCCESS);
          }
        }
        // then fall back to prefix matches on options
        if (!opt) {
          scanGroup(opts, true);
          if (!opt) {
            for (auto &g : groups) {
              scanGroup(*g, true);
            }
          }
        }
        // no prefix match on groups

        // TODO: refactor this to use std::vector and keep all matches
        // that way we can deal with ambiguity and emit a
        // "did you mean ..." message
        if (opt) {
          opt->appendHelpMessage(std::cout, 0, 0, 0, true);
          exit(EXIT_SUCCESS);
          return;
        }

        if (inp[0] == '#') {
          // try as an argument
          // #1 #2 ... are the args
          char *end   = nullptr;
          int   argIx = (int)strtol(inp + 1, &end, 10);
          if (*end == 0) {
            if (argIx - 1 < 0 || argIx - 1 >= (int)args.size()) {
              err("-h option: invalid argument index");
            } else {
              args[argIx - 1].appendHelpMessage(std::cout, 0, 0, 0, true);
              exit(EXIT_SUCCESS);
            }
          } // else e.g. "$1abc"
        } else {
          std::string str = "-h option: unknown option, argument,"
                            " or group ";
          err(str + inp);
        }
      } // end if *inp != 0
    }

public:
    bool parse(int argc, const char **argv, O &optVal)
    {
      int argIx = 1;

      // no arguments given ==> -h
      if (argIx == argc) {
        static const char *help[2] = {argv[0], "-h"};
        argc                       = sizeof(help) / sizeof(help[0]);
        argv                       = help;
      }

      ErrorHandler errHandler(exeName);
      while (argIx < argc) {
        bool matched = false;

        // try as global opt
        matched = opts.tryMatch(argc, argv, argIx, errHandler, optVal);
        if (!matched) {
          // try as grouped option
          for (auto &g : groups) {
            if (g->tryMatch(argc, argv, argIx, errHandler, optVal)) {
              matched = true;
              break;
            }
          }
        }

        // try as a group name match (-X is alias for -h=X)
        if (!matched && argv[argIx][0] == '-') {
          for (auto &g : groups) {
            if (streq(argv[argIx] + 1, g->prefix)) {
              // recurse
              std::string        helpArg     = std::string("-h=") + g->prefix;
              static const char *helpArgs[2] = {argv[0], helpArg.c_str()};
              return parse(2, helpArgs, optVal);
            }
          }
        }

        if (!matched) {
          if (strpfx("-", argv[argIx])) {
            errHandler(concat(argv[argIx], ": unmatched program option"));
            return false;
          }

          // try as arg
          for (auto &a : args) {
            if (a.tryMatch(argc, argv, argIx, errHandler, optVal)) {
              matched = true;
              break;
            }
          }
        }

        if (!matched) {
          errHandler(concat(argv[argIx], ": unmatched program argument"));
          return false;
        }
      }

      // set defaults and check for missing
      auto setDefaults = [&](Group<O> &g) {
        for (auto &o : g.members) {
          if (o.timesMatched == 0) {
            if (o.hasAttribute(OptAttrs::REQUIRED)) {
              o.setDefault(errHandler, optVal);
              //  } else {
              //    errHandler(concat(
              //      o.optName().c_str(),
              //      ": undefined\n",
              //      o.makeHelpMessage().c_str()));
            }
          }
        }
      };
      setDefaults(opts);
      for (auto &g : groups) {
        setDefaults(*g);
      }

      for (auto &o : args) {
        if (o.timesMatched == 0) {
          if (o.hasAttribute(OptAttrs::REQUIRED)) {
            o.setDefault(errHandler, optVal);
          }
        }
      }

      return true;
    }

private:
    void appendUsage(
        std::ostream &                 os,
        bool                           isTty,
        const char *                   exeName,
        const char *                   examples)
    {
      os << "usage: " << exeName << " OPTIONS ARGS"
         << "\n";
      os << "where OPTIONS:"
         << "\n";
      // autoscale all options
      opts.appendGroupSummary(os);
      int gCw = 4;
      for (auto &g : groups) {
        if (g->prefix) {
          gCw = std::max(gCw, (int)strlen(g->prefix) + 3);
        }
      }
      if (!groups.empty()) {
        os << "\n";
        for (auto &g : groups) {
          os << std::setw(3 + gCw) << std::left
            << concat("  -", g->prefix, "...");
          os << "  "; // spaces for option long names: should be lCw
          os << "  " << g->name << " (-" << g->prefix << " for more info)"
            << "\n";
        }
      }

      if (!args.empty()) {
        os << "\n";
        os << " and where ARGS:"
           << "\n";
        // autoscale arg type column
        int atCw = 8;
        for (auto &a : args) {
          atCw = std::max(atCw, a.typeName ? (int)strlen(a.typeName) : 0);
        }
        for (auto &a : args) {
          a.appendHelpMessage(os, gCw, 0, atCw, false);
          os << "\n";
        }
        if (examples) {
          os << "\n"
             << "EXAMPLES: "
             << "\n"
             << examples;
        }
      }
      if (!extraHelp.empty()) {
        os << "\n";
        os << "also see extra help sections:\n";
        int maxCw = 4;
        for (const auto &eh : extraHelp) {
          maxCw = std::max(maxCw, 3 + (int)eh.key.length());
        }
        for (const auto &eh : extraHelp) {
          os << std::setw(3 + gCw) << std::left
            << concat("  -h=", eh.key) << "\n";
        }
      }
    }
  }; // class CmdlineSpec

  static const char *exeName(const char *path)
  {
    const char *sfx = path + strlen(path) - 1;
    while (sfx > path) {
      if (*sfx == '/' || *sfx == '\\')
        return sfx + 1;
      sfx--;
    }
    return path;
  }
} // namespace opts

#endif // _OPTS_HPP_