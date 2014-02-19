/* This is the GetData model file for Coverity Scan.  It provides hints to
 * Coverity Scan's static code analysis.
 *
 * Although this looks like a C source file, it isn't meant to be compiled.
 * Expect to see stack variables being used without initialisation.
 */

#define assert(...) /* */
typedef struct {int error;} DIRFILE;
typedef struct {} gd_entry_t;

/* sets D->error to non-zero when it returns zero */
unsigned int _GD_GetSPF(DIRFILE *D, gd_entry_t *E)
{
  unsigned int spf;

  if (spf == 0) {
    assert(D->error != 0);
  }
  return spf;
}

/* only allocates memory if supplied no buffer */
char *gd_error_string(const DIRFILE *D, char *buffer, size_t buflen)
{
  if (buffer == 0)
    __coverity_alloc__(buffer);

  return buffer;
}


/* sets D->error to non-zero when it returns NULL */
char *_GD_MungeFrag(DIRFILE *D, const gd_entry_t *P, int me, const char *code,
    int *offset)
{
  char *new_code;
  if (new_code == 0) {
    assert(D->error != 0);
  }
  return new_code;
}

/* sets D->error to non-zero when it returns NULL */
gd_entry_t *_GD_FindField(const DIRFILE *D, const char *field_code,
    gd_entry_t *const *list, unsigned int u, int dealias, unsigned int *index)
{
  gd_entry_t *E;
  if (E == 0) {
    assert(D->error != 0);
  }
  return E;
}

/* either sets D->error to non-zero and returns NULL or else allocates memory */
void *_GD_Malloc(DIRFILE *D, size_t size)
{
  void *ptr;

  __coverity_alloc__(ptr);
  if (ptr == 0) {
    assert(D->error != 0);
  }
  return ptr;
}

/* When fdopendir returns non-NULL, it has stolen the descriptor */
typedef struct {int fd} DIR;
DIR *fdopendir(int fd)
{
  DIR *d;
  if (d) {
    d->fd = fd;
  }
  return d;
}

int closedir(DIR *d)
{
  __coverity_close__(d->fd);
}

/* doesn't return */
void zend_error(int type, const char *format, ...)
{
  __coverity_panic__();
}
