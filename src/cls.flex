%{

#include "lexemes.hpp"

#define YY_DECL cls::Lexeme yylex (yyscan_t yyscanner, unsigned &inp_off, unsigned &strlit_off)

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
<SLASH_STAR>[^*\n]+   { inp_off += (unsigned int)yyget_leng(yyscanner); } // eat comment in line chunks
<SLASH_STAR>"*"       { inp_off++; } // eat the lone star
<SLASH_STAR>\n        { inp_off++; }

<STRING_DBL>\"        { inp_off++;
                        BEGIN(INITIAL);
                        return cls::Lexeme::STRLIT; }
<STRING_DBL>\\.       { inp_off += 2; }
<STRING_DBL>.         { inp_off++; }

<STRING_SNG>\'        { inp_off++;
                        BEGIN(INITIAL);
                        return cls::Lexeme::STRLIT; }
<STRING_SNG>\\.       { inp_off += 2; }
<STRING_SNG>.         { inp_off++; }

\<                    return cls::Lexeme::LANGLE;
\>                    return cls::Lexeme::RANGLE;
\[                    return cls::Lexeme::LBRACK;
\]                    return cls::Lexeme::RBRACK;
\{                    return cls::Lexeme::LBRACE;
\}                    return cls::Lexeme::RBRACE;
\(                    return cls::Lexeme::LPAREN;
\)                    return cls::Lexeme::RPAREN;

\|                    return cls::Lexeme::PIPE;
\&                    return cls::Lexeme::AMP;
\.                    return cls::Lexeme::DOT;
\,                    return cls::Lexeme::COMMA;
\;                    return cls::Lexeme::SEMI;
\:                    return cls::Lexeme::COLON;
\`                    return cls::Lexeme::BACKTICK;
\~                    return cls::Lexeme::TILDE;

\!                    return cls::Lexeme::BANG;
\@                    return cls::Lexeme::AT;
\#                    return cls::Lexeme::HASH;
\=                    return cls::Lexeme::EQ;

\%                    return cls::Lexeme::MOD;
\*                    return cls::Lexeme::MUL;
\/                    return cls::Lexeme::DIV;
\+                    return cls::Lexeme::ADD;
\-                    return cls::Lexeme::SUB;
\<<                   return cls::Lexeme::LSH;
\>>                   return cls::Lexeme::RSH;
\"                    { strlit_off = inp_off;
                        inp_off += 1;
                        BEGIN(STRING_DBL);
                      }
\'                    { strlit_off = inp_off;
                        inp_off += 1;
                        BEGIN(STRING_SNG);
                      }


"/*"                  {inp_off += 2; BEGIN(SLASH_STAR);}

[a-zA-Z_][a-zA-Z_0-9]*  return cls::Lexeme::IDENT;

0[bB][01]+             return cls::Lexeme::INTLIT02;
[0-9]+                 return cls::Lexeme::INTLIT10;
0[xX][0-9A-Fa-f]+      return cls::Lexeme::INTLIT16;

[0-9]+\.[0-9]+([eE][-+]?[0-9]+)?  return cls::Lexeme::FLTLIT;
[0-9]+[eE][-+]?[0-9]+  return cls::Lexeme::FLTLIT;

\n                     { inp_off++; }
[ \t\r]+               { inp_off += (unsigned int)yyget_leng(yyscanner); } /* whitespace */;
"//"[^\n]*             { inp_off += (unsigned int)yyget_leng(yyscanner); } /* EOL comment ?*/



.                    return cls::Lexeme::LEXICAL_ERROR;
<<EOF>>              return cls::Lexeme::END_OF_FILE;

%%
