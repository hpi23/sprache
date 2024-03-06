#include "./libAnyObj.h"
#include "dynstring/dynstring.h"
#include "reflection.h"

AnyValue __hpi_internal_parse_json(DynString *input, void *(allocator)(), void(trace_allocation)(void * root, TypeDescriptor type, TypeDescriptor * type_ptr));
DynString *__hpi_internal_marshal_json(TypeDescriptor type, void *value);
