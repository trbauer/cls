#include "parser.hpp"

const char *cls::to_symbol(cls::lexemes::lexeme l)
{
#define CLS_LEXEME_TOKEN(T) case cls::lexemes::lexeme::T: return #T
  switch (l) {
    CLS_LEXEME_TOKEN(LEXICAL_ERROR);
    CLS_LEXEME_TOKEN(NEWLINE);

    CLS_LEXEME_TOKEN(LANGLE); // <
    CLS_LEXEME_TOKEN(RANGLE); // >
    CLS_LEXEME_TOKEN(LBRACK); // [
    CLS_LEXEME_TOKEN(RBRACK); // ]
    CLS_LEXEME_TOKEN(LBRACE); // {
    CLS_LEXEME_TOKEN(RBRACE); // }
    CLS_LEXEME_TOKEN(LPAREN); // (
    CLS_LEXEME_TOKEN(RPAREN); // )

    CLS_LEXEME_TOKEN(AMP);    // &
    CLS_LEXEME_TOKEN(PIPE);   // |
    CLS_LEXEME_TOKEN(CIRC);  // `
    CLS_LEXEME_TOKEN(DOT);    // .
    CLS_LEXEME_TOKEN(COMMA);  // ,
    CLS_LEXEME_TOKEN(SEMI);   // ;
    CLS_LEXEME_TOKEN(COLON);  // :
    CLS_LEXEME_TOKEN(BACKTICK);  // `

    CLS_LEXEME_TOKEN(TILDE);  // ~

    CLS_LEXEME_TOKEN(BANG);   // !
    CLS_LEXEME_TOKEN(AT);     // @
    CLS_LEXEME_TOKEN(HASH);   // #
    CLS_LEXEME_TOKEN(EQ);     // =

    CLS_LEXEME_TOKEN(MUL);    // *
    CLS_LEXEME_TOKEN(DIV);    // /
    CLS_LEXEME_TOKEN(MOD);    // %
    CLS_LEXEME_TOKEN(ADD);    // +
    CLS_LEXEME_TOKEN(SUB);    // -
    CLS_LEXEME_TOKEN(LSH);    // <<
    CLS_LEXEME_TOKEN(RSH);    // >>

    CLS_LEXEME_TOKEN(IDENT);  // [_a-zA-Z][_a-zA-Z0-9]*
    CLS_LEXEME_TOKEN(INTLIT02); // an integral pattern
    CLS_LEXEME_TOKEN(INTLIT10); // an integral pattern
    CLS_LEXEME_TOKEN(INTLIT16); // an integral pattern
    CLS_LEXEME_TOKEN(FLTLIT); // a floating point pattern

    CLS_LEXEME_TOKEN(END_OF_FILE);
#undef CLS_LEXEME_TOKEN
  default: return "?";
    //  default: {
    //    static char buf[16];
    //    sprintf(buf,"%d?",(int)l);
    //    return buf;
    //    }
  }
}

const char *cls::to_syntax(cls::lexemes::lexeme l)
{
#define CLS_LEXEME_TOKEN(T,S) case cls::lexemes::lexeme::T: return S;
  switch (l) {
  CLS_LEXEME_TOKEN(LEXICAL_ERROR,"ERROR");
  CLS_LEXEME_TOKEN(NEWLINE,"newline");

  CLS_LEXEME_TOKEN(LANGLE,"<"); // <
  CLS_LEXEME_TOKEN(RANGLE,">"); // >
  CLS_LEXEME_TOKEN(LBRACK,"["); // [
  CLS_LEXEME_TOKEN(RBRACK,"]"); // ]
  CLS_LEXEME_TOKEN(LBRACE,"{"); // {
  CLS_LEXEME_TOKEN(RBRACE,"}"); // }
  CLS_LEXEME_TOKEN(LPAREN,"("); // (
  CLS_LEXEME_TOKEN(RPAREN,")"); // )

  CLS_LEXEME_TOKEN(AMP,"&");    // &
  CLS_LEXEME_TOKEN(PIPE,"|");   // |
  CLS_LEXEME_TOKEN(CIRC,"^");   // ^
  CLS_LEXEME_TOKEN(DOT,".");    // .
  CLS_LEXEME_TOKEN(COMMA,",");  // ,
  CLS_LEXEME_TOKEN(SEMI,";");   // ;
  CLS_LEXEME_TOKEN(COLON,":");  // :
  CLS_LEXEME_TOKEN(BACKTICK,"`");  // :

  CLS_LEXEME_TOKEN(TILDE,"~");  // ~

  CLS_LEXEME_TOKEN(BANG,"!");   // !
  CLS_LEXEME_TOKEN(AT,"@");     // @
  CLS_LEXEME_TOKEN(HASH,"#");   // #
  CLS_LEXEME_TOKEN(EQ,"=");     // =

  CLS_LEXEME_TOKEN(MUL,"*");    // *
  CLS_LEXEME_TOKEN(DIV,"/");    // /
  CLS_LEXEME_TOKEN(MOD,"%");    // %
  CLS_LEXEME_TOKEN(ADD,"+");    // +
  CLS_LEXEME_TOKEN(SUB,"-");    // -
  CLS_LEXEME_TOKEN(LSH,"<<");    // <<
  CLS_LEXEME_TOKEN(RSH,">>");    // >>

  CLS_LEXEME_TOKEN(IDENT,"identifier");  // [_a-zA-Z][_a-zA-Z0-9]*
  CLS_LEXEME_TOKEN(INTLIT02,"int"); // an integral pattern
  CLS_LEXEME_TOKEN(INTLIT10,"int"); // an integral pattern
  CLS_LEXEME_TOKEN(INTLIT16,"int"); // an integral pattern
  CLS_LEXEME_TOKEN(FLTLIT,"float"); // a floating point pattern

  CLS_LEXEME_TOKEN(END_OF_FILE,"<<EOF>>");
#undef CLS_LEXEME_TOKEN
  default: return "?";
    //  default: {
    //    static char buf[16];
    //    sprintf(buf,"%d?",(int)l);
    //    return buf;
    //    }
  }
}

