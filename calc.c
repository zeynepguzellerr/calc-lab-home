// Zeynep Hatice Guzeller 231ADB274
// https://github.com/zeynep-guzeller/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used:https://chat.deepseek.com/share/p03jh5ad6esjpkbkqs 
//   token positions for error tracking, and the fseek+ftell file reading
//   pattern. Conversation history attached to the submission.

#include <dirent.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#define STUDENT_ID "231ADB274"
#define NAME_FULL  "zeynep_guzeller"
#define NAME_SHORT "zeynep"

enum TokenKind {
    TOKEN_END, TOKEN_NUMBER, TOKEN_LPAREN, TOKEN_RPAREN,
    TOKEN_PLUS, TOKEN_MINUS, TOKEN_STAR, TOKEN_SLASH, TOKEN_POW, TOKEN_BAD
};

typedef struct {
    int kind;
    long position;
    int is_float;
    long long int_value;
    double float_value;
} Token;

typedef struct {
    const char *file_buffer;
    long file_length;
    long current_pos;
    Token current;
    long error_pos;
} Parser;

typedef struct {
    int is_float;
    long long int_value;
    double float_value;
} Value;

static void next_token(Parser *p)
{
    while (p->current_pos < p->file_length) {
        char c = p->file_buffer[p->current_pos];
        if (c == ' ' || c == '\t' || c == '\r') p->current_pos++;
        else break;
    }

    // current_pos is 0-based and never reset, so +1 is always the true
    // 1-based file position, even after tabs and comment lines.
    p->current.position = p->current_pos + 1;
    p->current.is_float = 0;
    p->current.int_value = 0;
    p->current.float_value = 0.0;

    if (p->current_pos >= p->file_length) {
        p->current.kind = TOKEN_END;
        return;
    }

    char c = p->file_buffer[p->current_pos];

    if (c == '\n') {
        // The newline itself is the "missing operand at end of line"
        // position, so we just report p->current.position.
        p->current.kind = TOKEN_END;
        return;
    }

    if (c >= '0' && c <= '9') {
        long start = p->current_pos;
        while (p->current_pos < p->file_length &&
               p->file_buffer[p->current_pos] >= '0' &&
               p->file_buffer[p->current_pos] <= '9') p->current_pos++;

        int is_float = 0;
        if (p->current_pos < p->file_length && p->file_buffer[p->current_pos] == '.') {
            // A dot must be followed by a digit, otherwise the dot itself
            // is the invalid character.
            if (p->current_pos + 1 >= p->file_length ||
                p->file_buffer[p->current_pos + 1] < '0' ||
                p->file_buffer[p->current_pos + 1] > '9') {
                p->current.kind = TOKEN_BAD;
                p->current.position = p->current_pos + 1;
                return;
            }
            is_float = 1;
            p->current_pos++;
            while (p->current_pos < p->file_length &&
                   p->file_buffer[p->current_pos] >= '0' &&
                   p->file_buffer[p->current_pos] <= '9') p->current_pos++;
        }

        if (is_float) {
            double v = 0.0, scale = 1.0;
            int after_dot = 0;
            for (long k = start; k < p->current_pos; k++) {
                char d = p->file_buffer[k];
                if (d == '.') { after_dot = 1; continue; }
                v = v * 10.0 + (double)(d - '0');
                if (after_dot) scale *= 10.0;
            }
            p->current.kind = TOKEN_NUMBER;
            p->current.is_float = 1;
            p->current.float_value = v / scale;
        } else {
            long long v = 0;
            for (long k = start; k < p->current_pos; k++)
                v = v * 10 + (p->file_buffer[k] - '0');
            p->current.kind = TOKEN_NUMBER;
            p->current.int_value = v;
        }
        return;
    }

    if (c == '(') { p->current.kind = TOKEN_LPAREN; p->current_pos++; return; }
    if (c == ')') { p->current.kind = TOKEN_RPAREN; p->current_pos++; return; }
    if (c == '+') { p->current.kind = TOKEN_PLUS;   p->current_pos++; return; }
    if (c == '-') { p->current.kind = TOKEN_MINUS;  p->current_pos++; return; }
    if (c == '/') { p->current.kind = TOKEN_SLASH;  p->current_pos++; return; }

    if (c == '*') {
        if (p->current_pos + 1 < p->file_length &&
            p->file_buffer[p->current_pos + 1] == '*') {
            p->current.kind = TOKEN_POW;
            p->current_pos += 2;
        } else {
            p->current.kind = TOKEN_STAR;
            p->current_pos++;
        }
        return;
    }

    p->current.kind = TOKEN_BAD;
}

static void set_error(Parser *p, long position)
{
    if (p->error_pos == 0) p->error_pos = position;
}

static Value make_int(long long x)
{
    Value v;
    v.is_float = 0;
    v.int_value = x;
    v.float_value = 0.0;
    return v;
}

static Value make_float(double x)
{
    Value v;
    v.is_float = 1;
    v.int_value = 0;
    v.float_value = x;
    return v;
}

static double as_double(Value v)
{
    return v.is_float ? v.float_value : (double)v.int_value;
}

static Value parse_expr(Parser *p);

static Value parse_primary(Parser *p)
{
    if (p->error_pos != 0) return make_int(0);

    if (p->current.kind == TOKEN_NUMBER) {
        Value v;
        v.is_float = p->current.is_float;
        v.int_value = p->current.int_value;
        v.float_value = p->current.float_value;
        next_token(p);
        return v;
    }

    if (p->current.kind == TOKEN_LPAREN) {
        next_token(p);
        Value v = parse_expr(p);
        if (p->error_pos != 0) return v;
        if (p->current.kind != TOKEN_RPAREN) {
            // Missing ')' at end of line, or something else where ')'
            // should be. Both point at the token we are looking at.
            set_error(p, p->current.position);
            return v;
        }
        next_token(p);
        return v;
    }

    // A number or '(' was expected but we got something else. Reporting
    // at the current token covers a second operator in a row, an invalid
    // character, a missing operand at end of line, and an extra ')' all
    // at once.
    set_error(p, p->current.position);
    return make_int(0);
}

static Value apply_binary(Parser *p, Value a, Value b, int op,
                          long operator_pos, long rhs_pos)
{
    // If both operands are integers the whole operation stays in long long.
    if (!a.is_float && !b.is_float) {
        if (op == TOKEN_SLASH) {
            if (b.int_value == 0) {
                // Division by zero is reported at the START of the right
                // operand, so rhs_pos was saved before parsing it.
                set_error(p, rhs_pos);
                return make_int(0);
            }
            return make_int(a.int_value / b.int_value);
        }
        if (op == TOKEN_STAR) return make_int(a.int_value * b.int_value);
        if (op == TOKEN_PLUS) return make_int(a.int_value + b.int_value);
        return make_int(a.int_value - b.int_value);
    }

    // Otherwise at least one side is a float, so use double arithmetic.
    double x = as_double(a), y = as_double(b), r;
    if (op == TOKEN_SLASH) {
        if (y == 0.0) {
            set_error(p, rhs_pos);
            return make_float(0.0);
        }
        r = x / y;
    } else if (op == TOKEN_STAR) r = x * y;
    else if (op == TOKEN_PLUS)   r = x + y;
    else                         r = x - y;

    if (isinf(r) || isnan(r)) {
        // A bad float result is reported at the operator itself.
        set_error(p, operator_pos);
        return make_float(0.0);
    }
    return make_float(r);
}

static Value parse_power(Parser *p)
{
    Value base = parse_primary(p);
    if (p->error_pos != 0) return base;
    if (p->current.kind != TOKEN_POW) return base;

    long operator_pos = p->current.position;
    next_token(p);

    // AI (ChatGPT): idea of saving the position before the recursive call,
    // so I can report it later if the exponent is negative.
    long rhs_pos = p->current.position;

    // Calling parse_power() again makes '**' right-associative:
    // 2**3**2 becomes 2**(3**2) = 512.
    Value exponent = parse_power(p);
    if (p->error_pos != 0) return base;

    if (!base.is_float && !exponent.is_float) {
        if (exponent.int_value < 0) {
            // Negative integer exponent: report where the exponent starts.
            set_error(p, rhs_pos);
            return make_int(0);
        }
        long long result = 1;
        for (long long k = 0; k < exponent.int_value; k++)
            result *= base.int_value;
        return make_int(result);
    }

    double r = pow(as_double(base), as_double(exponent));
    if (isinf(r) || isnan(r)) {
        set_error(p, operator_pos);
        return make_float(0.0);
    }
    return make_float(r);
}

static Value parse_term(Parser *p)
{
    Value left = parse_power(p);
    while (p->error_pos == 0 &&
           (p->current.kind == TOKEN_STAR || p->current.kind == TOKEN_SLASH)) {
        int op = p->current.kind;
        long operator_pos = p->current.position;
        next_token(p);
        long rhs_pos = p->current.position;
        Value right = parse_power(p);
        if (p->error_pos != 0) return left;
        left = apply_binary(p, left, right, op, operator_pos, rhs_pos);
    }
    return left;
}

static Value parse_expr(Parser *p)
{
    Value left = parse_term(p);
    while (p->error_pos == 0 &&
           (p->current.kind == TOKEN_PLUS || p->current.kind == TOKEN_MINUS)) {
        int op = p->current.kind;
        long operator_pos = p->current.position;
        next_token(p);
        long rhs_pos = p->current.position;
        Value right = parse_term(p);
        if (p->error_pos != 0) return left;
        left = apply_binary(p, left, right, op, operator_pos, rhs_pos);
    }
    return left;
}

typedef struct {
    char *data;
    size_t length;
    size_t capacity;
} TextBuffer;

static void buffer_add(TextBuffer *b, const char *s)
{
    size_t n = strlen(s);
    if (b->length + n + 1 > b->capacity) {
        size_t cap = b->capacity ? b->capacity : 256;
        while (cap < b->length + n + 1) cap *= 2;
        char *bigger = realloc(b->data, cap);
        if (!bigger) {
            fprintf(stderr, "error: out of memory\n");
            exit(1);
        }
        b->data = bigger;
        b->capacity = cap;
    }
    memcpy(b->data + b->length, s, n);
    b->length += n;
    b->data[b->length] = '\0';
}

static char *read_file(const char *path, long *out_length)
{
    FILE *f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "error: cannot open %s: %s\n", path, strerror(errno));
        return NULL;
    }

    // AI (ChatGPT): fseek(SEEK_END) + ftell + one malloc to read the whole
    // file. Error handling and the trailing '\0' are my own.
    if (fseek(f, 0, SEEK_END) != 0) {
        fclose(f);
        fprintf(stderr, "error: cannot seek in %s\n", path);
        return NULL;
    }
    long size = ftell(f);
    if (size < 0) {
        fclose(f);
        fprintf(stderr, "error: cannot get size of %s\n", path);
        return NULL;
    }
    rewind(f);

    char *buf = malloc((size_t)size + 1);
    if (!buf) {
        fclose(f);
        fprintf(stderr, "error: out of memory\n");
        return NULL;
    }

    size_t got = fread(buf, 1, (size_t)size, f);
    fclose(f);
    if ((long)got != size) {
        free(buf);
        fprintf(stderr, "error: could not read all of %s\n", path);
        return NULL;
    }

    buf[size] = '\0';
    *out_length = size;
    return buf;
}

static void extract_base_name(const char *path, char *out, size_t out_size, int strip_txt)
{
    size_t len = strlen(path);
    while (len > 0 && path[len - 1] == '/') len--;
    size_t start = len;
    while (start > 0 && path[start - 1] != '/') start--;

    size_t n = len - start;
    if (strip_txt && n > 4 && strcmp(path + start + n - 4, ".txt") == 0) n -= 4;
    if (n >= out_size) n = out_size - 1;

    memcpy(out, path + start, n);
    out[n] = '\0';
}

static int join_path(char *out, size_t out_size, const char *folder, const char *name)
{
    int n = snprintf(out, out_size, "%s/%s", folder, name);
    if (n < 0 || (size_t)n >= out_size) {
        fprintf(stderr, "error: path is too long: %s/%s\n", folder, name);
        return 0;
    }
    return 1;
}

static int build_output_name(char *out, size_t out_size, const char *base)
{
    int n = snprintf(out, out_size, "%s_%s_%s.txt", base, NAME_FULL, STUDENT_ID);
    if (n < 0 || (size_t)n >= out_size) {
        fprintf(stderr, "error: output file name is too long\n");
        return 0;
    }
    return 1;
}

static int ensure_folder(const char *path)
{
    struct stat st;
    if (stat(path, &st) == 0) {
        if (S_ISDIR(st.st_mode)) return 1;
        fprintf(stderr, "error: %s exists but is not a folder\n", path);
        return 0;
    }
    if (mkdir(path, 0777) != 0) {
        fprintf(stderr, "error: cannot create folder %s: %s\n", path, strerror(errno));
        return 0;
    }
    return 1;
}

static int process_file(const char *input_path, const char *out_dir, const char *base)
{
    long length = 0;
    char *file_buffer = read_file(input_path, &length);
    if (!file_buffer) return 0;

    TextBuffer out = { NULL, 0, 0 };
    buffer_add(&out, "");

    long error_pos = 0;
    long index = 0;

    while (index < length && error_pos == 0) {
        long line_start = index;
        long line_end = index;
        while (line_end < length && file_buffer[line_end] != '\n') line_end++;

        long first = line_start;
        while (first < line_end &&
               (file_buffer[first] == ' ' || file_buffer[first] == '\t' ||
                file_buffer[first] == '\r')) first++;

        // Blank lines and '#' comment lines produce no output.
        if (first < line_end && file_buffer[first] != '#') {
            Parser p;
            p.file_buffer = file_buffer;
            p.file_length = length;
            p.current_pos = line_start;
            p.error_pos = 0;

            next_token(&p);
            Value v = parse_expr(&p);

            // Anything left on the line (like an extra ')') is an error.
            if (p.error_pos == 0 && p.current.kind != TOKEN_END)
                set_error(&p, p.current.position);

            if (p.error_pos != 0) {
                error_pos = p.error_pos;
            } else {
                char number[64];
                if (v.is_float) snprintf(number, sizeof number, "%.15g", v.float_value);
                else            snprintf(number, sizeof number, "%lld", v.int_value);
                buffer_add(&out, number);
                buffer_add(&out, "\n");
            }
        }

        index = (line_end < length) ? line_end + 1 : line_end;
    }

    int ok = 1;
    char name[512], out_path[4096];

    if (!build_output_name(name, sizeof name, base) ||
        !join_path(out_path, sizeof out_path, out_dir, name)) {
        ok = 0;
    } else {
        FILE *f = fopen(out_path, "wb");
        if (!f) {
            fprintf(stderr, "error: cannot write %s: %s\n", out_path, strerror(errno));
            ok = 0;
        } else {
            if (error_pos != 0) fprintf(f, "ERROR:%ld\n", error_pos);
            else                fwrite(out.data, 1, out.length, f);
            fclose(f);
        }
    }

    free(out.data);
    free(file_buffer);
    return ok;
}

static int process_folder(const char *folder_path, const char *out_dir)
{
    DIR *folder = opendir(folder_path);
    if (!folder) {
        fprintf(stderr, "error: cannot open folder %s: %s\n", folder_path, strerror(errno));
        return 0;
    }

    int ok = 1;
    struct dirent *entry;
    while (ok && (entry = readdir(folder)) != NULL) {
        size_t n = strlen(entry->d_name);
        if (n < 5 || strcmp(entry->d_name + n - 4, ".txt") != 0) continue;

        char full[4096];
        if (!join_path(full, sizeof full, folder_path, entry->d_name)) {
            ok = 0;
            break;
        }

        // Skip subfolders, even if their name ends with ".txt".
        struct stat st;
        if (stat(full, &st) != 0 || !S_ISREG(st.st_mode)) continue;

        char base[512];
        extract_base_name(entry->d_name, base, sizeof base, 1);
        if (!process_file(full, out_dir, base)) ok = 0;
    }
    closedir(folder);
    return ok;
}

int main(int argc, char **argv)
{
    // AI (ChatGPT): for-loop + strcmp pattern for short and long flags.
    // Error messages and edge cases are my own.
    const char *dir_arg = NULL;
    const char *out_arg = NULL;
    const char *file_arg = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "error: %s needs a folder name\n", argv[i]);
                return 1;
            }
            dir_arg = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "error: %s needs a folder name\n", argv[i]);
                return 1;
            }
            out_arg = argv[++i];
        } else if (argv[i][0] == '-' && argv[i][1] != '\0') {
            fprintf(stderr, "error: unknown option %s\n", argv[i]);
            return 1;
        } else {
            if (file_arg) {
                fprintf(stderr, "error: only one input file is allowed\n");
                return 1;
            }
            file_arg = argv[i];
        }
    }

    if (!dir_arg && !file_arg) {
        fprintf(stderr, "usage: calc [-d DIR | --dir DIR] "
                        "[-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
        return 1;
    }
    if (dir_arg && file_arg) {
        fprintf(stderr, "error: -d cannot be combined with a single file\n");
        return 1;
    }

    char base[512], default_dir[1024];
    const char *out_dir = out_arg;
    int ok;

    if (dir_arg) {
        struct stat st;
        if (stat(dir_arg, &st) != 0 || !S_ISDIR(st.st_mode)) {
            fprintf(stderr, "error: cannot open folder %s\n", dir_arg);
            return 1;
        }

        extract_base_name(dir_arg, base, sizeof base, 0);

        if (!out_dir) {
            int n = snprintf(default_dir, sizeof default_dir,
                             "%s_%s_%s", base, NAME_SHORT, STUDENT_ID);
            if (n < 0 || (size_t)n >= sizeof default_dir) {
                fprintf(stderr, "error: folder name is too long\n");
                return 1;
            }
            out_dir = default_dir;
        }

        if (!ensure_folder(out_dir)) return 1;
        ok = process_folder(dir_arg, out_dir);
    } else {
        FILE *test = fopen(file_arg, "rb");
        if (!test) {
            fprintf(stderr, "error: cannot open %s\n", file_arg);
            return 1;
        }
        fclose(test);

        extract_base_name(file_arg, base, sizeof base, 1);

        if (!out_dir) {
            int n = snprintf(default_dir, sizeof default_dir,
                             "%s_%s_%s", base, NAME_SHORT, STUDENT_ID);
            if (n < 0 || (size_t)n >= sizeof default_dir) {
                fprintf(stderr, "error: folder name is too long\n");
                return 1;
            }
            out_dir = default_dir;
        }

        if (!ensure_folder(out_dir)) return 1;
        ok = process_file(file_arg, out_dir, base);
    }

    return ok ? 0 : 1;
}