/* -- imports -- */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <ctype.h>
#include <stdarg.h>

/* -- types -- */
#define CATIS_TYPE_BOOL   (1<<0)
#define CATIS_TYPE_INT    (1<<1)
#define CATIS_TYPE_LIST   (1<<2)
#define CATIS_TYPE_STRING (1<<3)
#define CATIS_TYPE_SYMBOL (1<<4)
#define CATIS_TYPE_TUPLE  (1<<5)
#define CATIS_TYPE_ANY    INT_MAX

/* -- object representation -- */
typedef struct catis_object {
    int type; // CATIS_TYPE_
    int reference_count;
    int line;
    union {
        int integer;
        int boolean;
        struct {
            struct catis_object** element;
            size_t length;
            int quoted; // for tuples to know if caputuring or not
        } list_or_tuple;
        struct {
            char* pointer;
            size_t length;
            int quoted; // for symbols to know if evaluating or not
        } string_or_symbol;
    };
} catis_object;


/* -- procedure representation -- */
// to reference before implementing
struct catis_context;

typedef struct catis_procedure {
    const char* name;
    catis_object* procedure; // if NULL then is a C procedure
    int (*c_procedure)(struct catis_context*);
    struct catis_procedure* next;
} catis_procedure;

/* -- stack frames for local variables -- */
#define CATIS_MAX_LOCALVARS 256
typedef struct stackframe {
    catis_object* locals[CATIS_MAX_LOCALVARS];
    catis_procedure* procedure;
    int line;
    struct stackframe* previous;
} stackframe;

/* -- intrepret context -- */
#define CATIS_ERROR_STRING_LENGTH 256
typedef struct catis_context {
    size_t stack_length;
    catis_object** stack;
    catis_procedure* procedure;
    stackframe* frame;
    char error_string[CATIS_ERROR_STRING_LENGTH]; // to stock error messages
} catis_context;

// to reference before implementing
void set_error(
    catis_context* context,
    const char* pointer,
    const char* message
);
catis_procedure* lookup_procedure(catis_context* context, const char* name);
void load_library(catis_context* context);

/* -- out of memory utils -- */
void* catis_allocate(size_t size) {
    void* pointer = malloc(size);
    if (!pointer) {
        fprintf(stderr, "Out of memory allocating %zu bytes\n", size);
        exit(1);
    }
    return pointer;
}

void* catis_reallocate(void* old_pointer, size_t size) {
    void* pointer = realloc(old_pointer, size);
    if (!pointer) {
        fprintf(stderr, "Out of memory allocating %zu bytes\n", size);
        exit(1);
    }
    return pointer;
}

/* -- object -- */
void release(catis_object* object) {
    if (object == NULL) return;
    assert(object->reference_count >= 0); 
    if (--object->reference_count == 0) {
        switch (object->type) {
            case CATIS_TYPE_LIST:
            case CATIS_TYPE_TUPLE:
                for (size_t i = 0; i < object->list_or_tuple.length; i++) {
                    release(object->list_or_tuple.element[i]);
                }
                free(object->list_or_tuple.element);
                break;
            case CATIS_TYPE_STRING:
            case CATIS_TYPE_SYMBOL:
                free(object->string_or_symbol.pointer);
                break;
            default:
                break;
        }
        free(object);
    }
}

void retain(catis_object* object) {
    object->reference_count++;
}

catis_object* new_object(int type) {
    catis_object* object = catis_allocate(sizeof(*object));
    object->reference_count = 1;
    object->type = type;
    object->line = 0;
    return object;
}

/* -- lexing and parsing -- */
int is_symbol(int character) {
    if (isalpha(character)) {
        return 1;
    }
    switch (character) {
        case '@':
        case '$':
        case '+':
        case '-':
        case '*':
        case '/':
        case '=':
        case '?':
        case '%':
        case '>':
        case '<':
        case '_':
        case '\'':
            return 1;
        default:
            return 0;
    }
}

const char* consume_space_and_comment(const char* string, int* line) {
    while (1) {
        while (isspace(string[0])) {
            if (string[0] == '\n' && line) {
                (*line)++;
            }
            string++;
        }
        if (!(string[0] == '/' && string[1] == '/')) {
            break;
        }
        while (string[0] && string[0] != '\n') {
            string++;
        }
    }
    return string;
}

catis_object* parse_object(
    catis_context* context,
    const char* string,
    const char** next,
    int* line
) {
    catis_object* object = new_object(-1);

    string = consume_space_and_comment(string, line);

    if (line) {
        object->line = *line;
    }

    // parse integer
    if ((string[0] == '-' && isdigit(string[1])) || isdigit(string[0])) {
        char buffer[64];
        size_t length = 0;
        while (
            (*string == '-' || isdigit(*string)) &&
            length < sizeof(buffer) - 1
        ) {
            buffer[length++] = *string++;
        }
        buffer[length] = 0;
        object->type = CATIS_TYPE_INT;
        object->integer = atoi(buffer);
        if (next) {
            *next = string;
        }
    }

    // parse list, tuple, quoted tuple
    else if (
        string[0] == '[' ||
        string[0] == '(' ||
        (string[0] == '\'' && string[1] == '(')
    ) {
        if (string[0] == '\'') {
            object->list_or_tuple.quoted = 1;
            string++;
        } else {
            object->list_or_tuple.quoted = 0;
        }
        object->type = string[0] == '[' ? CATIS_TYPE_LIST : CATIS_TYPE_TUPLE;
        object->list_or_tuple.length = 0;
        object->list_or_tuple.element = NULL;
        string++;

        while (1) {
            string = consume_space_and_comment(string, line);
            if (
                (object->type == CATIS_TYPE_LIST  && string[0] == ']') ||
                (object->type == CATIS_TYPE_TUPLE && string[0] == ')')
            ) {
                if (next) {
                    *next = string + 1;
                }
                return object;
            }

            const char* next_pointer;
            catis_object* element = parse_object(
                context,
                string,
                &next_pointer,
                line
            );

            if (element == NULL) {
                release(object);
                return NULL;
            }
            else if (
                object->type == CATIS_TYPE_TUPLE &&
                (element->type != CATIS_TYPE_SYMBOL ||
                 element->string_or_symbol.length != 1)
            ) {
                release(element);
                release(object);
                set_error(
                    context,
                    string,
                    "Tuples can only contain single character symbols"
                );
                return NULL;
            }

            object->list_or_tuple.element = catis_reallocate(
                object->list_or_tuple.element,
                sizeof(catis_object*) * (object->list_or_tuple.length+1)
            );
            object->list_or_tuple.element[
                object->list_or_tuple.length++
            ] = element;

            string = next_pointer;
            continue;
        }

        set_error(context, string, "List never closed");
        release(object);
        return NULL;
    }

    // parse symbol
    else if (is_symbol(string[0])) {
        object->type = CATIS_TYPE_SYMBOL;
        if(string[0] == '\'') {
            object->string_or_symbol.quoted = 1;
            string++;
        }
        else {
            object->string_or_symbol.quoted = 0;
        }
        const char* end = string;
        while (is_symbol(*end)) {
            end++;
        }
        object->string_or_symbol.length = end - string;
        char* destination = catis_allocate(
            object->string_or_symbol.length + 1
        );
        object->string_or_symbol.pointer = destination;
        memcpy(destination, string, object->string_or_symbol.length);
        destination[object->string_or_symbol.length] = 0;

        if (next) {
            *next = end;
        }
    }

    // parse boolean
    else if (string[0] == '#') {
        if (string[1] != 't' && string[1] != 'f') {
            set_error(
                context,
                string,
                "Booleans are either #t or #f"
            );
            release(object);
            return NULL;
        }
        object->type = CATIS_TYPE_BOOL;
        object->boolean = string[1] == 't' ? 1 : 0;
        string += 2;

        if (next) {
            *next = string;
        }
    }

    // parse string
    else if (string[0] == '"') {
        string++;
        object->type = CATIS_TYPE_STRING;
        object->string_or_symbol.pointer = catis_allocate(1);
        object->string_or_symbol.length = 0;

        while (string[0] && string[0] != '"') {
            int character = string[0];
            switch (character) {
                case '\\':
                    string++;
                    character = string[0];
                    switch (character) {
                        case 'n':
                            character = '\n';
                            break;
                        case 'r':
                            character = '\r';
                            break;
                        case 't':
                            character = '\t';
                            break;
                        default:
                            break;
                    }
                default:
                    break;
            }

            object->string_or_symbol.pointer = catis_reallocate(
                object->string_or_symbol.pointer,
                object->string_or_symbol.length + 2
            );
            object->string_or_symbol.pointer[
                object->string_or_symbol.length++
            ] = character;
            string++;
        }

        if (string[0] != '"') {
            set_error(
                context,
                string,
                "Quotation marks never closed in string"
            );
            release(object);
            return NULL;
        }

        object->string_or_symbol.pointer[object->string_or_symbol.length] = 0;
        string++;

        if (next) {
            *next = string;
        }
    }

    else {
        set_error(context, string, "No object type starts like this");
        release(object);
        return NULL;
    }

    return object;
}

/* -- compare objects -- */
#define COMPARE_TYPE_MISMATCH INT_MIN
int compare(catis_object* a, catis_object* b) {
    // integer
    if (a->type == CATIS_TYPE_INT && b->type == CATIS_TYPE_INT) {
        if (a->integer < b->integer) {
            return -1;
        }
        if (a->integer > b->integer) {
            return 1;
        }
        return 0;
    }

    // boolean
    else if (a->type == CATIS_TYPE_BOOL && b->type == CATIS_TYPE_BOOL) {
        if (a->boolean < b->boolean) {
            return -1;
        }
        if (a->boolean > b->boolean) {
            return 1;
        }
        return 0;
    }

    // string or symbol
    else if (
        (a->type == CATIS_TYPE_STRING || a->type == CATIS_TYPE_SYMBOL) &&
        (b->type == CATIS_TYPE_STRING || b->type == CATIS_TYPE_SYMBOL)
    ) {
        int comparaison = strcmp(
            a->string_or_symbol.pointer,
            b->string_or_symbol.pointer
        );
        return comparaison < 0 ? -1 : (comparaison > 0 ? 1 : 0);
    }

    // list or tuple
    else if (
        (a->type == CATIS_TYPE_LIST || a->type == CATIS_TYPE_TUPLE) &&
        (b->type == CATIS_TYPE_LIST || b->type == CATIS_TYPE_TUPLE)
    ) {
        if (a->list_or_tuple.length < b->list_or_tuple.length) {
            return -1;
        }
        if (a->list_or_tuple.length > b->list_or_tuple.length) {
            return 1;
        }
        return 0;
    }

    else {
        return COMPARE_TYPE_MISMATCH;
    }

    return COMPARE_TYPE_MISMATCH;
}

/* -- sorting utils -- */
int quicksort_object_comparaison(const void* a, const void* b) {
    catis_object** object_a = (catis_object**)a;
    catis_object** object_b = (catis_object**)b;
    return compare(object_a[0], object_b[0]);
}

/* -- printing utils -- */
#define PRINT_RAW       0
#define PRINT_COLOR (1<<0)
#define PRINT_REPR  (1<<1)

void print_object(catis_object* object, int flags) {
    const char* escape;
    int color = flags & PRINT_COLOR;
    int repr  = flags & PRINT_REPR;

    if (color) {
        switch (object->type) {
            case CATIS_TYPE_LIST:
                escape = "\033[33;1m"; // yellow
                break;
            case CATIS_TYPE_TUPLE:
                escape = "\033[34;1m"; // blue
                break;
            case CATIS_TYPE_SYMBOL:
                escape = "\033[36;1m"; // cyan
                break;
            case CATIS_TYPE_STRING:
                escape = "\033[32;1m"; // green
                break;
            case CATIS_TYPE_INT:
                escape = "\033[37;1m"; // gray
                break;
            case CATIS_TYPE_BOOL:
                escape = "\033[35;1m"; // gray
                break;
        }
        printf("%s", escape);
    }

    switch (object->type) {
        case CATIS_TYPE_BOOL:
            printf("#%c", object->boolean ? 't' : 'f');
            break;
case CATIS_TYPE_INT:
            printf("%d", object->integer);
            break;
        case CATIS_TYPE_SYMBOL:
            printf("%s", object->string_or_symbol.pointer);
            break;
        case CATIS_TYPE_STRING:
            if (!repr) {
                fwrite(
                    object->string_or_symbol.pointer,
                    object->string_or_symbol.length,
                    1,
                    stdout
                );
            } else {
                printf("\"");
                for (size_t i = 0; i < object->string_or_symbol.length; i++) {
                    int character = object->string_or_symbol.pointer[i];
                    switch (character) {
                        case '\n':
                            printf("\\n");
                            break;
                        case '\r':
                            printf("\\r");
                            break;
                        case '\t':
                            printf("\\t");
                            break;
                        case '"':
                            printf("\\\"");
                            break;
                        default:
                            printf("%c", character);
                            break;
                    }
                }
                printf("\"");
            }
            break;
        case CATIS_TYPE_LIST:
        case CATIS_TYPE_TUPLE:
            if (repr) {
                printf("%c", object->type == CATIS_TYPE_LIST ? '[' : '(');
            }
            for (size_t i = 0; i < object->list_or_tuple.length; i++) {
                print_object(object->list_or_tuple.element[i], flags);
                if (i != object->list_or_tuple.length - 1) {
                    printf(" ");
                }
            }
            if (color) {
                printf("%s", escape);
            }
            if (repr) {
                printf("%c", object->type == CATIS_TYPE_LIST ? ']' : ')');
            }
            break;
    }
    if (color) {
        printf("\033[0m");
    }
}

/* -- object constructor -- */
catis_object* new_integer(int integer) {
    catis_object* object = new_object(CATIS_TYPE_INT);
    object->integer = integer;
    return object;
}

catis_object* new_boolean(int boolean) {
    catis_object* object = new_object(CATIS_TYPE_BOOL);
    object->boolean = boolean;
    return object;
}

catis_object* new_string(const char* string, size_t length) {
    catis_object* object = new_object(CATIS_TYPE_STRING);
    object->string_or_symbol.length = length;
    object->string_or_symbol.pointer = catis_allocate(length + 1);
    memcpy(object->string_or_symbol.pointer, string, length);
    object->string_or_symbol.pointer[length] = 0;
    return object;
}

catis_object* deep_copy(catis_object* object) {
    if (object == NULL) {
        return NULL;
    }

    catis_object* copy = new_object(object->type);
    switch (object->type) {
        case CATIS_TYPE_INT:
            copy->integer = object->integer;
            break;
        case CATIS_TYPE_BOOL:
            copy->boolean = object->boolean;
            break;
        case CATIS_TYPE_LIST:
        case CATIS_TYPE_TUPLE:
            copy->list_or_tuple.length = object->list_or_tuple.length;
            copy->list_or_tuple.element = catis_allocate(
                sizeof(catis_object*) *object->list_or_tuple.length
            );
            for (size_t i = 0; i < object->list_or_tuple.length; i++) {
                copy->list_or_tuple.element[i] = deep_copy(
                    object->list_or_tuple.element[i]
                );
            }
            break;
        case CATIS_TYPE_STRING:
        case CATIS_TYPE_SYMBOL:
            copy->string_or_symbol.length = object->string_or_symbol.length;
            copy->string_or_symbol.quoted = object->string_or_symbol.quoted;
copy->string_or_symbol.pointer = catis_allocate(
                object->string_or_symbol.length + 1
            );
            memcpy(
                copy->string_or_symbol.pointer,
                object->string_or_symbol.pointer,
                object->string_or_symbol.length + 1
            );
            break;
    }

    return copy;
}

catis_object* get_unshared_object(catis_object* object) {
    if (object->reference_count > 1) {
        release(object);
        return deep_copy(object);
    } else {
        return object;
    }
}

/* -- error utils -- */
void set_error(
    catis_context* context,
    const char* pointer,
    const char* message
) {
    if (!context) {
        return;
    }

    if (!pointer) {
        pointer = context->frame->procedure ?
            context->frame->procedure->name :
            "unknow context";
    }

    size_t length = snprintf(
        context->error_string,
        CATIS_ERROR_STRING_LENGTH,
        "%s: '%.30s%s'",
        message,
        pointer,
        strlen(pointer) > 30 ? "..." : ""
    );

    stackframe* frame = context->frame;
    while (frame && length < CATIS_ERROR_STRING_LENGTH) {
        length += snprintf(
            context->error_string + length,
            CATIS_ERROR_STRING_LENGTH - length,
            " in %s:%d ",
            frame->procedure ?
            frame->procedure->name :
            "unknown",
            frame->line
        );
        frame = frame->previous;
    }
}

/* -- stack frame utils -- */
stackframe* new_stackframe(catis_context* context) {
    stackframe* frame = catis_allocate(sizeof(*frame));
    memset(frame->locals, 0, sizeof(frame->locals));
    frame->procedure = NULL;
    frame->previous = context ? context->frame : NULL;
    return frame;
}

void release_stackframe(stackframe* frame) {
    for (int i = 0; i < CATIS_MAX_LOCALVARS; i++) {
        release(frame->locals[i]);
    }
    free(frame);
}

/* -- interpreter constructor -- */
catis_context* new_interpreter(void) {
    catis_context* interpreter = catis_allocate(sizeof(*interpreter));
    interpreter->stack_length = 0;
    interpreter->stack = NULL;
    interpreter->procedure = NULL;
    interpreter->frame = new_stackframe(NULL);
    load_library(interpreter);
    return interpreter;
}

/* -- stack utils -- */
void stack_push(catis_context* context, catis_object* object) {
    context->stack = catis_reallocate(
        context->stack,
        sizeof(catis_object*) * (context->stack_length + 1)
    );
    context->stack[context->stack_length++] = object;
}

catis_object* stack_pop(catis_context* context) {
    if (context->stack_length == 0) {
        return NULL;
    } else {
        return context->stack[--context->stack_length];
    }
}

catis_object* stack_peek(catis_context* context, size_t offset) {
    if (context->stack_length <= offset) {
        return NULL;
    } else {
        return context->stack[
            context->stack_length - (offset + 1)
        ];
    }
}

void stack_set(catis_context* context, size_t offset, catis_object* object) {
    assert(context->stack_length > offset);
    context->stack[context->stack_length - (offset + 1)] = object;
}

#define STACK_SHOW_MAX_ELEMENTS 16
void stack_show(catis_context* context) {
    ssize_t i = context->stack_length - STACK_SHOW_MAX_ELEMENTS;
    if (i < 0) {
        i = 0;
    }
    while (i < (ssize_t)context->stack_length) {
        catis_object* object = context->stack[i];
        print_object(object, PRINT_COLOR | PRINT_REPR);
        printf(" ");
        i++;
    }
    if (context->stack_length > STACK_SHOW_MAX_ELEMENTS) {
        printf("[... %zu more objects ...]", i);
    }
    if (context->stack_length) {
        printf("\n");
    }
}

/* -- eval -- */
int eval(catis_context* context, catis_object* list) {
    assert(list->type == CATIS_TYPE_LIST);

    for (size_t i = 0; i < list->list_or_tuple.length; i++) {
        catis_object* object = list->list_or_tuple.element[i];
        catis_procedure* procedure;
        context->frame->line = object->line;

        switch (object->type) {
            case CATIS_TYPE_TUPLE:
                if (object->list_or_tuple.quoted) {
                    catis_object* tuple = deep_copy(object);
                    tuple->list_or_tuple.quoted = 0;
                    stack_push(context, tuple);
                    break;
                }

                // capture variables
                if (context->stack_length < object->list_or_tuple.length) {
                    set_error(
                        context,
                        object->list_or_tuple.element[
                            context->stack_length
                        ]->string_or_symbol.pointer,
                        "Out of stack while capturing local"
                    );
                    return 1;
                }

                context->stack_length -= object->list_or_tuple.length;
                for (size_t i = 0; i < object->list_or_tuple.length; i++) {
                    int index =
                        object->list_or_tuple.element[i]
                            ->string_or_symbol.pointer[0];
                    release(context->frame->locals[index]);
                    context->frame->locals[index] =
                        context->stack[context->stack_length + i];
                }
                break;
            case CATIS_TYPE_SYMBOL:
                if (object->string_or_symbol.quoted) {
                    catis_object* symbol = deep_copy(object);
                    symbol->string_or_symbol.quoted = 0;
                    stack_push(context, symbol);
                    break;
                }

                if (object->string_or_symbol.pointer[0] == '$') {
                    int index = object->string_or_symbol.pointer[1];
                    if (context->frame->locals[index] == NULL) {
                        set_error(
                            context,
                            object->string_or_symbol.pointer,
                            "Unbound local variable"
                        );
                        return 1;
                    }
                    stack_push(context, context->frame->locals[index]);
                    retain(context->frame->locals[index]);
                }
                else {
                    procedure = lookup_procedure(
                        context,
                        object->string_or_symbol.pointer
                    );
                    if (procedure == NULL) {
                        set_error(
                            context,
                            object->string_or_symbol.pointer,
                            "Symbol nout bound to procedure"
                        );
                        return 1;
                    }
                    if (procedure->c_procedure) {
                        catis_procedure* previous = context->frame->procedure;
                        context->frame->procedure = procedure;
                        int error = procedure->c_procedure(context);
                        context->frame->procedure = previous;
                        if (error) {
                            return error;
                        }
                    }
                    // catis procedure
                    else {
                        stackframe* previous = context->frame;
                        context->frame = new_stackframe(context);
                        context->frame->procedure = procedure;
                        int error = eval(context, procedure->procedure);
                        release_stackframe(context->frame);
                        context->frame = previous;
                        if (error) {
                            return error;
                        }
                    }
                }
                break;
            default:
                stack_push(context, object);
                retain(object);
                break;
        }
    }

    return 0;
}

/* -- procedure utils -- */
int check_stack_length(catis_context* context, size_t minimum) {
    if (context->stack_length < minimum) {
        set_error(context, NULL, "Out of stack");
        return 1;
    }
    return 0;
}

int check_stack_type(catis_context* context, size_t count, ...) {
    if (check_stack_length(context, count)) {
        return 1;
    }
    va_list types;
    va_start(types, count);
    for (size_t i = 0; i < count; i++) {
        int type = va_arg(types, int);
        if (
            !(type & context->stack[context->stack_length - (count - i)]->type)
        ) {
            set_error(context, NULL, "Type mismatch");
            return 1;
        }
    }
    va_end(types);
    return 0;
}

catis_procedure* lookup_procedure(catis_context* context, const char* name) {
    catis_procedure* this = context->procedure;
    while (this) {
        if (!strcmp(this->name, name)) {
            return this;
        }
        this = this->next;
    }
    return NULL;
}

catis_procedure* new_procedure(catis_context* context, const char* name) {
    catis_procedure* procedure = catis_allocate(sizeof(*procedure));
    procedure->name = catis_allocate(strlen(name) + 1);
    memcpy((char*)procedure->name, name, strlen(name) + 1);
    procedure->next = context->procedure;
    context->procedure = procedure;
    return procedure;
}

// TODO: ligne 778


/* -- main -- */
int main() {
    return 0;
} 
