#include "./libAnyObj.h"

void anyobj_insert(AnyObject * obj, char * key, AnyValue value) {
    AnyValue * value_heap = (AnyValue *) malloc(sizeof(AnyValue));
    *value_heap = value;
    hashmap_insert(obj->fields, key, value_heap);
}

AnyObject * anyobj_new() {
    AnyObject * obj = malloc(sizeof(AnyObject));
    obj->fields = hashmap_new();
    return obj;
}
