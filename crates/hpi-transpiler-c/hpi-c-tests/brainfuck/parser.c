#include "./parser.h"
#include <stdlib.h>

char *parser_next(Parser *parser) {
    TokenResult result = lexer_next(parser->lexer);
    if (result.error != NULL)  {
        return result.error;
    }
    parser->curr_tok = result.token;
    return NULL;
}

Parser *parser_new(char *input) {
    Lexer *lexer = lexer_new(input);
    Parser parser = { .lexer = lexer };
    parser_next(&parser);
    Parser * parser_temp = malloc(sizeof(Parser));
    return parser_temp;
}

ParseResult parser_parse(Parser *parser) {
    ParseResult result = {.error = NULL};

    char * error  = parser_next(parser);
    if (error != NULL) {
        result.error = error;
        return result;
    }
}
