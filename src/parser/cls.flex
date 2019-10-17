%{

#include "lexemes.hpp"

#define YY_DECL cls::lexemes::lexeme yylex (yyscan_t yyscanner, unsigned &inp_off, unsigned &strlit_off)

/*
 * It seems many versions of flex don't support column info in re-entrant
 * scanners.  This works around the issue.
 */
#define YY_USER_ACTION \
    yyset_column(yyget_column(yyscanner) + (int)yyget_leng(yyscanner), yyscanner);

%}

/*
 * generated via:
 * % flex Lexer.flex
 * originally designed on flex 2.5.37
 */

%option outfile="cls_lex.yy.cpp" header-file="cls_lex.yy.hpp"
%option nounistd
%option reentrant
%option noyywrap
%option yylineno
/* omits isatty */
%option never-interactive

/*
TODO:
%option extra-type="struct stat *"
If extra context needed:
   void yyset_extra(YY_EXTRA_TYPE arbitrary_data, yyscan_t scanner);
*/

%x SLASH_STAR
%x STRING_DBL
%x STRING_SNG
%%

<SLASH_STAR>"*/"      { inp_off += 2; BEGIN(INITIAL); }
<SLASH_STAR>[^*\n]+   { inp_off += (unsigned)yyget_leng(yyscanner); } // eat comment in line chunks
<SLASH_STAR>"*"       { inp_off++; } // eat the lone star
<SLASH_STAR>\n        { inp_off++; }

<STRING_DBL>\"        { inp_off++;
                        BEGIN(INITIAL);
                        return cls::lexemes::STRLIT; }
<STRING_DBL>\\.       { inp_off += 2; }
<STRING_DBL>.         { inp_off++; }

<STRING_SNG>\'        { inp_off++;
                        BEGIN(INITIAL);
                        return cls::lexemes::STRLIT; }
<STRING_SNG>\\.       { inp_off += 2; }
<STRING_SNG>.         { inp_off++; }

\<                    return cls::lexemes::LANGLE;
\>                    return cls::lexemes::RANGLE;
\[                    return cls::lexemes::LBRACK;
\]                    return cls::lexemes::RBRACK;
\{                    return cls::lexemes::LBRACE;
\}                    return cls::lexemes::RBRACE;
\(                    return cls::lexemes::LPAREN;
\)                    return cls::lexemes::RPAREN;

\|                    return cls::lexemes::PIPE;
\&                    return cls::lexemes::AMP;
\^                    return cls::lexemes::CIRC;
\.                    return cls::lexemes::DOT;
\,                    return cls::lexemes::COMMA;
\;                    return cls::lexemes::SEMI;
\:                    return cls::lexemes::COLON;
\`                    return cls::lexemes::BACKTICK;
\~                    return cls::lexemes::TILDE;

\!                    return cls::lexemes::BANG;
\@                    return cls::lexemes::AT;
\#                    return cls::lexemes::HASH;
\=                    return cls::lexemes::EQ;
\?                    return cls::lexemes::QUESTION;
\$                    return cls::lexemes::DOLLAR;

\%                    return cls::lexemes::MOD;
\*                    return cls::lexemes::MUL;
\/                    return cls::lexemes::DIV;
\+                    return cls::lexemes::ADD;
\-                    return cls::lexemes::SUB;
\<<                   return cls::lexemes::LSH;
\>>                   return cls::lexemes::RSH;
\"                    { strlit_off = inp_off;
                        inp_off += 1;
                        BEGIN(STRING_DBL);
                      }
\'                    { strlit_off = inp_off;
                        inp_off += 1;
                        BEGIN(STRING_SNG);
                      }


"/*"                  {inp_off += 2; BEGIN(SLASH_STAR);}

[a-zA-Z_][a-zA-Z_0-9]*  return cls::lexemes::IDENT;

0[bB][01]+             return cls::lexemes::INTLIT02;
[0-9]+                 return cls::lexemes::INTLIT10;
0[xX][0-9A-Fa-f]+      return cls::lexemes::INTLIT16;

[0-9]+\.[0-9]+([eE][-+]?[0-9]+)?[fF]? return cls::lexemes::FLTLIT;
[0-9]+[eE][-+]?[0-9]+[fF]             return cls::lexemes::FLTLIT;

\n                     { return cls::lexemes::NEWLINE; }
[ \t\r]+               { inp_off += (unsigned)yyget_leng(yyscanner); } /* whitespace */;
"//"[^\n]*             { inp_off += (unsigned)yyget_leng(yyscanner); } /* EOL comment */



.                    return cls::lexemes::LEXICAL_ERROR;
<<EOF>>              return cls::lexemes::END_OF_FILE;

%%
