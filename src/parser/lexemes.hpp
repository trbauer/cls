#ifndef PARSER_LEXEMES_HPP
#define PARSER_LEXEMES_HPP

namespace cls
{
  namespace lexemes {
    enum lexeme {
      LEXICAL_ERROR = 0, // Windows GDI #define's "ERROR"
      NEWLINE,

      // delimiters
      LANGLE, // <
      RANGLE, // >
      LBRACK, // [
      RBRACK, // ]
      LBRACE, // {
      RBRACE, // }
      LPAREN, // (
      RPAREN, // )

      // separators
      AMP,   // &
      PIPE,  // |
      CIRC,  // ^ (circumflex)
      DOT,   // .
      COMMA, // ,
      SEMI,  // ;
      COLON, // :
      BACKTICK, // `

      // source modifiers and saturation
      // NEG (see SUB), // -       negation (src modifier)
      TILDE, // ~       bitwise complement

      // reserved symbols
      BANG, // !
      AT,   // @
      HASH, // #
      EQ,   // =

      // operators (reserved for future arithmetic)
      MUL, // *
      DIV, // /
      MOD, // %
      ADD, // +
      SUB, // -
      LSH, // <<
      RSH, // >>

      // variable lexemes
      IDENT,    // e.g. myVar
      INTLIT02, // 0b1101
      INTLIT10, // 13
      INTLIT16, // 0x123
      FLTLIT,   // 1.3, 0.1, 1e9, 1e-4,
      STRLIT,   // "foo"
      DIMENSION, // 128x128 128x128x8

      END_OF_FILE, // special lexeme that indicates end of file

      NUM_LEXEMES
    }; // enum lexeme
  } // namespace lexemes
} // namespace cls

#endif