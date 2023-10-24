#include "./libAnyObj.h"
#include "../hpi-c-tests/dynstring/dynstring.h"

AnyValue __hpi_internal_parse_json(DynString *input);
DynString *__hpi_internal_marshal_json(TypeDescriptor type, void * value);
