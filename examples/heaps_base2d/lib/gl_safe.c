// Safe wrapper for glGetShaderInfoLog on macOS.
// The macOS OpenGL driver crashes in glGetShaderInfoLog when querying
// the error log of a shader that failed to compile with an unsupported
// GLSL version (e.g., #version 130). This wrapper checks
// GL_INFO_LOG_LENGTH first and skips the call if the log is empty.
//
// Usage: DYLD_INSERT_LIBRARIES=libgl_safe.dylib ./ash_cli game.hl
// Or link with -Wl,-interposable

#include <OpenGL/gl3.h>
#include <string.h>
#include <stdio.h>

// The real function from OpenGL.framework
extern void glGetShaderInfoLog(GLuint shader, GLsizei bufSize,
                                GLsizei *length, GLchar *infoLog);

// Our safe replacement
void safe_glGetShaderInfoLog(GLuint shader, GLsizei bufSize,
                              GLsizei *length, GLchar *infoLog) {
    // First check if there's actually a log to retrieve
    GLint log_length = 0;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_length);

    if (log_length <= 0) {
        // No log available — return empty string instead of crashing
        if (length) *length = 0;
        if (infoLog && bufSize > 0) infoLog[0] = '\0';
        return;
    }

    // Log exists and should be safe to query
    glGetShaderInfoLog(shader, bufSize, length, infoLog);
}

// Same for program info log (just in case)
extern void glGetProgramInfoLog(GLuint program, GLsizei bufSize,
                                 GLsizei *length, GLchar *infoLog);

void safe_glGetProgramInfoLog(GLuint program, GLsizei bufSize,
                               GLsizei *length, GLchar *infoLog) {
    GLint log_length = 0;
    glGetProgramiv(program, GL_INFO_LOG_LENGTH, &log_length);

    if (log_length <= 0) {
        if (length) *length = 0;
        if (infoLog && bufSize > 0) infoLog[0] = '\0';
        return;
    }

    glGetProgramInfoLog(program, bufSize, length, infoLog);
}

// DYLD interpose table
typedef struct {
    const void *replacement;
    const void *replacee;
} interpose_t;

__attribute__((used))
static const interpose_t interpose_funcs[]
    __attribute__((section("__DATA,__interpose"))) = {
    { (const void *)safe_glGetShaderInfoLog,
      (const void *)glGetShaderInfoLog },
    { (const void *)safe_glGetProgramInfoLog,
      (const void *)glGetProgramInfoLog },
};
