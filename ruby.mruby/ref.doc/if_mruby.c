/* vi:set ts=2 sts=2 sw=2:
 *
 * VIM - Vi IMproved    by Bram Moolenaar
 *
 * MRuby interface by Yasuhiro Matsumoto
 *
 * Do ":help uganda"  in Vim to read copying and usage conditions.
 * Do ":help credits" in Vim to see a list of people who contributed.
 * See README.txt for an overview of the Vim source code.
 */

#ifdef HAVE_CONFIG_H
# include "auto/config.h"
#endif

#include <stdio.h>
#include <string.h>

#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/class.h>
#include <mruby/proc.h>
#include <mruby/data.h>
#include <mruby/array.h>
#include <mruby/hash.h>
#include <mruby/string.h>
#include <mruby/variable.h>

#include "vim.h"
#include "version.h"

static mrb_state* mrb = NULL;
static struct mrbc_context* context = NULL;
static mrb_value objtbl;
static struct RObject* mrb_stdout;

static struct RClass* mVIM;
static struct RClass* cBuffer;
static struct RClass* cVimWindow;
static struct RClass* eDeletedBufferError;
static struct RClass* eDeletedWindowError;

static int ensure_mruby_initialized(void);
static void error_print();
static void mruby_io_init(void);
static void mruby_vim_init(void);

static void
mrb_vim_free(mrb_state *mrb, void *p) {
}

static const struct mrb_data_type VIM_Window_type = {
  "VIM::Window", mrb_vim_free,
};

static const struct mrb_data_type VIM_Buffer_type = {
  "VIM::Buffer", mrb_vim_free,
};

void
mruby_end()
{
  if (context) {
    mrbc_context_free(mrb, context);
    context = NULL;
  }

  if (mrb) {
    mrb_close(mrb);
    mrb = NULL;
  }
}
//
static mrb_value
eval_string(const char *str)
{
  mrb_value result;
  struct mrb_parser_state *parser;
  int ai = mrb_gc_arena_save(mrb);
  parser = mrb_parser_new(mrb);
  parser->s = str;
  parser->send = str + strlen(str);
  parser->lineno = 1;
  mrb_parser_parse(parser, context);
  result = mrb_nil_value();
  if (0 < parser->nerr) {
    MSG(parser->error_buffer[0].message);
  } else {
    int n = mrb_generate_code(mrb, parser);
    result = mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
  }

  mrb_gc_arena_restore(mrb, ai);
  mrb_parser_free(parser);
  return result;
}

void
ex_mruby(exarg_T *eap)
{
  char *script = NULL;

  script = (char *)script_get(eap, eap->arg);
  if (!eap->skip && ensure_mruby_initialized()) {
    eval_string(script ? script : (char *)eap->arg);
    if (mrb->exc)
      error_print();
  }

  vim_free(script);
}

void ex_mrubydo(exarg_T *eap)
{
  linenr_T i;

  if (ensure_mruby_initialized())
  {
    if (u_save(eap->line1 - 1, eap->line2 + 1) != OK)
      return;

    for (i = eap->line1; i <= eap->line2; i++) {
      mrb_value line;

      line = mrb_str_new_cstr(mrb, (char *)ml_get(i));
      mrb_vm_const_set(mrb, mrb_intern(mrb, "$_"), line);
      line = eval_string((char *) eap->arg);
      if (mrb->exc) {
        error_print();
        break;
      }

      line = mrb_vm_const_get(mrb, mrb_intern(mrb, "$_"));
      if (!mrb_nil_p(line)) {
        if (mrb_type(line) != MRB_TT_STRING) {
          EMSG(_("E265: $_ must be an instance of String"));
          return;
        }

        ml_replace(i, (char_u *) mrb_string_value_ptr(mrb, line), 1);
        changed();
#ifdef SYNTAX_HL
        syn_changed(i); /* recompute syntax hl. for this line */
#endif
      }
    }
    check_cursor();
    update_curbuf(NOT_VALID);
  }
}

void ex_mrubyfile(exarg_T *eap)
{
  if (ensure_mruby_initialized())
  {
    FILE* fp = fopen((char *) eap->arg, "rb");
    if (fp) {
      mrbc_filename(mrb, context, (char *) eap->arg);
      mrb_load_file_cxt(mrb, fp, context);
      fclose(fp);
      if (mrb->exc)
        error_print();
    } else {
      EMSG("Cannot open file");
    }
  }
}

void mruby_buffer_free(buf_T *buf)
{
  if (buf->b_mruby_ref)
  {
    mrb_funcall(mrb, objtbl, "[]=", 2,
        mrb_fixnum_value(mrb_obj_id(*(mrb_value*) buf->b_mruby_ref)),
        mrb_nil_value());

    ((struct RData*) buf->b_mruby_ref)->data = NULL;
  }
}

void mruby_window_free(win_T *win)
{
  if (win->w_mruby_ref)
  {
    mrb_funcall(mrb, objtbl, "[]=", 2,
        mrb_fixnum_value(mrb_obj_id(*(mrb_value*) win->w_mruby_ref)),
        mrb_nil_value());
    ((struct RData*) win->w_mruby_ref)->data = NULL;
  }
}

static int ensure_mruby_initialized()
{
  if (!mrb)
  {
    mrb = mrb_open();
    if (mrb) 
    {
      context = mrbc_context_new(mrb);
      context->capture_errors = 1;
      mruby_io_init();
      mruby_vim_init();
    }
  }

  return mrb != NULL;
}

static void
error_print()
{
  EMSG(mrb_string_value_ptr(mrb, mrb_obj_value(mrb->exc)));
  mrb->exc = 0;
}

static mrb_value vim_message(mrb_state *mrb, mrb_value self)
{
  char *buff, *p;
  mrb_value str;
  int ai = mrb_gc_arena_save(mrb);

  mrb_get_args(mrb, "o", &str);

  str = mrb_obj_as_string(mrb, str);
  if (RSTRING_LEN(str) > 0) {
    MSG(RSTRING_PTR(str));
  } else {
    MSG("");
  }

  mrb_gc_arena_restore(mrb, ai);
  return mrb_nil_value();
}

static mrb_value vim_set_option(mrb_state* mrb, mrb_value self)
{
  mrb_value str;
  mrb_get_args(mrb, "S", &str);
  do_set((char_u *) mrb_string_value_ptr(mrb, str), 0);
  update_screen(NOT_VALID);
  return mrb_nil_value();
}

static mrb_value vim_command(mrb_state* mrb, mrb_value self)
{
  mrb_value str;
  mrb_get_args(mrb, "S", &str);
  do_cmdline_cmd((char_u *) mrb_string_value_ptr(mrb, str));
  return mrb_nil_value();
}

#ifdef FEAT_EVAL
static mrb_value vim_to_mruby(typval_T *tv)
{
  mrb_value result = mrb_nil_value();

  if (tv->v_type == VAR_STRING)
  {
    result = mrb_str_new_cstr(mrb, tv->vval.v_string == NULL
        ? "" : (char *)(tv->vval.v_string));
  }
  else if (tv->v_type == VAR_NUMBER)
  {
    result = mrb_fixnum_value(tv->vval.v_number);
  }
# ifdef FEAT_FLOAT
  else if (tv->v_type == VAR_FLOAT)
  {
    result = mrb_float_value(tv->vval.v_float);
  }
# endif
  else if (tv->v_type == VAR_LIST)
  {
    list_T      *list = tv->vval.v_list;
    listitem_T  *curr;

    result = mrb_ary_new(mrb);

    if (list != NULL)
    {
      for (curr = list->lv_first; curr != NULL; curr = curr->li_next)
      {
        mrb_ary_push(mrb, result, vim_to_mruby(&curr->li_tv));
      }
    }
  }
  else if (tv->v_type == VAR_DICT)
  {
    result = mrb_hash_new(mrb);

    if (tv->vval.v_dict != NULL)
    {
      hashtab_T   *ht = &tv->vval.v_dict->dv_hashtab;
      long_u      todo = ht->ht_used;
      hashitem_T  *hi;
      dictitem_T  *di;

      for (hi = ht->ht_array; todo > 0; ++hi)
      {
        if (!HASHITEM_EMPTY(hi))
        {
          --todo;

          di = dict_lookup(hi);
          mrb_funcall(mrb, result, "[]=", 2,
              mrb_str_new_cstr(mrb, (char *)hi->hi_key),
              vim_to_mruby(&di->di_tv));
        }
      }
    }
  } /* else return mrb_nil_value(); */

  return result;
}
#endif

static mrb_value vim_evaluate(mrb_state* mrb, mrb_value self)
{
#ifdef FEAT_EVAL
  typval_T *tv;
  mrb_value result;
  mrb_value str;
  mrb_get_args(mrb, "S", &str);

  tv = eval_expr((char_u *) mrb_string_value_ptr(mrb, str), NULL);
  if (tv == NULL)
  {
    return mrb_nil_value();
  }

  result = vim_to_mruby(tv);

  free_tv(tv);

  return result;
#else
  return mrb_nil_value();
#endif
}

static mrb_value buffer_new(buf_T *buf)
{
  if (buf->b_mruby_ref)
  {
    return mrb_obj_value((struct RData*) buf->b_mruby_ref);
  }
  else
  {
    struct RData* obj = mrb_data_object_alloc(mrb, cBuffer, buf, &VIM_Buffer_type);
    buf->b_mruby_ref = (void *) obj;
    mrb_funcall(mrb, objtbl, "[]=", 2,
      mrb_fixnum_value(mrb_obj_id(mrb_obj_value(obj))),
      mrb_obj_value(obj));

    return mrb_obj_value(obj);
  }
}

static buf_T *get_buf(mrb_value obj)
{
  buf_T *buf;

  Data_Get_Struct(mrb, obj, &VIM_Buffer_type, buf);
  if (buf == NULL)
    mrb_raise(mrb, eDeletedBufferError, "attempt to refer to deleted buffer");

  return buf;
}

static mrb_value buffer_s_current(mrb_state* mrb, mrb_value self)
{
  return buffer_new(curbuf);
}

static mrb_value buffer_s_count(mrb_state* mrb, mrb_value self)
{
  buf_T *b;
  int n = 0;

  for (b = firstbuf; b != NULL; b = b->b_next)
  {
    /*  Deleted buffers should not be counted
     *    SegPhault - 01/07/05 */
    if (b->b_p_bl)
      n++;
  }

  return mrb_fixnum_value(n);
}

static mrb_value buffer_s_aref(mrb_state* mrb, mrb_value self)
{
  mrb_value num;
  mrb_get_args(mrb, "i", &num);
  int n = mrb_fixnum(num);

  buf_T *b;
  for (b = firstbuf; b != NULL; b = b->b_next)
  {
    /*  Deleted buffers should not be counted
          SegPhault - 01/07/05 */
    if (!b->b_p_bl)
      continue;

    if (n == 0)
      return buffer_new(b);

    n--;
  }

  return mrb_nil_value();
}

static mrb_value buffer_name(mrb_state* mrb, mrb_value self)
{
  buf_T *buf = get_buf(self);

  return buf->b_ffname 
    ? mrb_str_new_cstr(mrb, (char *)buf->b_ffname) 
    : mrb_nil_value();
}

static mrb_value buffer_number(mrb_state* mrb, mrb_value self)
{
  buf_T *buf = get_buf(self);

  return mrb_fixnum_value(buf->b_fnum);
}

static mrb_value buffer_count(mrb_state* mrb, mrb_value self)
{
  buf_T *buf = get_buf(self);

  return mrb_fixnum_value(buf->b_ml.ml_line_count);
}

static mrb_value get_buffer_line(buf_T *buf, linenr_T n)
{
  if (n <= 0 || n > buf->b_ml.ml_line_count)
    mrb_raisef(mrb, E_INDEX_ERROR, 
        "line number %ld out of range", (long)n);

  return mrb_str_new_cstr(mrb, (char *)ml_get_buf(buf, n, FALSE));
}

static mrb_value buffer_aref(mrb_state* mrb, mrb_value self)
{
  buf_T *buf = get_buf(self);

  if (buf != NULL) 
  {
    mrb_value num;
    mrb_get_args(mrb, "i", &num);
    return get_buffer_line(buf, (linenr_T)mrb_fixnum(num));
  }

  return mrb_nil_value(); /* For stop warning */
}

static mrb_value set_buffer_line(buf_T *buf, linenr_T n, mrb_value str)
{
  char    *line = mrb_string_value_ptr(mrb, str);
  aco_save_T    aco;

  if (n > 0 && n <= buf->b_ml.ml_line_count && line != NULL)
  {
    /* set curwin/curbuf for "buf" and save some things */
    aucmd_prepbuf(&aco, buf);

    if (u_savesub(n) == OK) {
      ml_replace(n, (char_u *)line, TRUE);
      changed();
#ifdef SYNTAX_HL
      syn_changed(n); /* recompute syntax hl. for this line */
#endif
    }

    /* restore curwin/curbuf and a few other things */
    aucmd_restbuf(&aco);
    /* Careful: autocommands may have made "buf" invalid! */

    update_curbuf(NOT_VALID);
  }
  else
  {
    mrb_raisef(mrb, E_INDEX_ERROR, "line number %ld out of range", (long)n);
  }

  return str;
}

static mrb_value buffer_aset(mrb_state* mrb, mrb_value self)
{
  buf_T *buf = get_buf(self);
  mrb_value num, str;

  mrb_get_args(mrb, "iS", &num, &str);
  if (buf != NULL)
    return set_buffer_line(buf, (linenr_T)mrb_fixnum(num), str);

  return str;
}

static mrb_value buffer_delete(mrb_state* mrb, mrb_value self)
{
  buf_T    *buf = get_buf(self);
  mrb_value num;
  long    n;
  aco_save_T    aco;

  mrb_get_args(mrb, "i", &num);
  n = mrb_fixnum(num);
  if (n > 0 && n <= buf->b_ml.ml_line_count)
  {
    /* set curwin/curbuf for "buf" and save some things */
    aucmd_prepbuf(&aco, buf);

    if (u_savedel(n, 1) == OK) {
      ml_delete(n, 0);

      /* Changes to non-active buffers should properly refresh
       *   SegPhault - 01/09/05 */
      deleted_lines_mark(n, 1L);

      changed();
    }

    /* restore curwin/curbuf and a few other things */
    aucmd_restbuf(&aco);
    /* Careful: autocommands may have made "buf" invalid! */

    update_curbuf(NOT_VALID);
  }
  else
  {
    mrb_raisef(mrb, E_INDEX_ERROR, "line number %ld out of range", n);
  }

  return mrb_nil_value();
}

static mrb_value buffer_append(mrb_state* mrb, mrb_value self)
{
  buf_T    *buf = get_buf(self);
  mrb_value num, str;
  char    *line;
  long    n;
  aco_save_T    aco;

  mrb_get_args(mrb, "iS", &num, &str);
  line = mrb_string_value_ptr(mrb, str);
  n = mrb_fixnum(num);
  if (line == NULL)
  {
    mrb_raise(mrb, E_INDEX_ERROR, "NULL line");
  }
  else if (n >= 0 && n <= buf->b_ml.ml_line_count)
  {
    /* set curwin/curbuf for "buf" and save some things */
    aucmd_prepbuf(&aco, buf);

    if (u_inssub(n + 1) == OK) {
      ml_append(n, (char_u *) line, (colnr_T) 0, FALSE);

      /*  Changes to non-active buffers should properly refresh screen
       *    SegPhault - 12/20/04 */
      appended_lines_mark(n, 1L);

      changed();
    }

    /* restore curwin/curbuf and a few other things */
    aucmd_restbuf(&aco);
    /* Careful: autocommands may have made "buf" invalid! */

    update_curbuf(NOT_VALID);
  }
  else
  {
    mrb_raisef(mrb, E_INDEX_ERROR, "line number %ld out of range", n);
  }

  return str;
}

static mrb_value window_new(win_T *win)
{
  if (win->w_mruby_ref)
  {
    return mrb_obj_value((struct RData*) win->w_mruby_ref);
  }
  else
  {
    struct RData* obj = mrb_data_object_alloc(mrb, cVimWindow, win, &VIM_Window_type);
    win->w_mruby_ref = (void *) obj;
    mrb_funcall(mrb, objtbl, "[]=", 2,
      mrb_fixnum_value(mrb_obj_id(mrb_obj_value(obj))),
      mrb_obj_value(obj));

    return mrb_obj_value(obj);
  }
}

static win_T *get_win(mrb_value obj)
{
  win_T *win;

  Data_Get_Struct(mrb, obj, &VIM_Window_type, win);
  if (win == NULL)
    mrb_raise(mrb, eDeletedWindowError, "attempt to refer to deleted window");

  return win;
}

static mrb_value window_s_current(mrb_state* mrb, mrb_value self)
{
  return window_new(curwin);
}

/*
 * Added line manipulation functions
 *    SegPhault - 03/07/05
 */
static mrb_value line_s_current(mrb_state* mrb, mrb_value self)
{
  return get_buffer_line(curbuf, curwin->w_cursor.lnum);
}

static mrb_value set_current_line(mrb_state* mrb, mrb_value self)
{
  mrb_value str;
  mrb_get_args(mrb, "S", &str);
  return set_buffer_line(curbuf, curwin->w_cursor.lnum, str);
}

static mrb_value current_line_number(mrb_state* mrb, mrb_value self)
{
  return mrb_fixnum_value((int)curwin->w_cursor.lnum);
}


static mrb_value window_s_count(mrb_state* mrb, mrb_value self)
{
#ifdef FEAT_WINDOWS
  win_T    *w;
  int n = 0;

  for (w = firstwin; w != NULL; w = w->w_next)
    n++;

  return mrb_fixnum_value(n);
#else
  return mrb_fixnum_value(1);
#endif
}

static mrb_value window_s_aref(mrb_state* mrb, mrb_value self)
{
  win_T *w;
  int n;
  mrb_value num;

  mrb_get_args(mrb, "i", &num);
  n = mrb_fixnum(num);
#ifndef FEAT_WINDOWS
  w = curwin;
#else
  for (w = firstwin; w != NULL; w = w->w_next, --n)
#endif
    if (n == 0)
      return window_new(w);

  return mrb_nil_value();
}

static mrb_value window_buffer(mrb_state* mrb, mrb_value self)
{
  win_T *win = get_win(self);

  return buffer_new(win->w_buffer);
}

static mrb_value window_height(mrb_state* mrb, mrb_value self)
{
  win_T *win = get_win(self);

  return mrb_fixnum_value(win->w_height);
}

static mrb_value window_set_height(mrb_state* mrb, mrb_value self)
{
  win_T *win = get_win(self);
  win_T *savewin = curwin;
  mrb_value height;

  mrb_get_args(mrb, "i", &height);
  curwin = win;
  win_setheight(mrb_fixnum(height));
  curwin = savewin;

  return height;
}

static mrb_value window_width(mrb_state* mrb, mrb_value self)
{
  win_T *win = get_win(self);

  return mrb_fixnum_value(win->w_width);
}

static mrb_value window_set_width(mrb_state* mrb, mrb_value self)
{
  win_T *win = get_win(self);
  win_T *savewin = curwin;
  mrb_value width;

  mrb_get_args(mrb, "i", &width);
  curwin = win;
  win_setwidth(mrb_fixnum(width));
  curwin = savewin;
  return width;
}

static mrb_value window_cursor(mrb_value self)
{
  win_T *win = get_win(self);

  return mrb_assoc_new(mrb, 
      mrb_fixnum_value(win->w_cursor.lnum), 
      mrb_fixnum_value(win->w_cursor.col));
}

static mrb_value window_set_cursor(mrb_state* mrb, mrb_value self)
{
  mrb_value lnum, col;
  win_T *win = get_win(self);
  mrb_value pos;

  mrb_get_args(mrb, "A", &pos);
  mrb_check_type(mrb, pos, MRB_TT_ARRAY);
  if (RARRAY_LEN(pos) != 2)
    mrb_raise(mrb, E_ARGUMENT_ERROR, "array length must be 2");

  lnum = RARRAY_PTR(pos)[0];
  col = RARRAY_PTR(pos)[1];
  win->w_cursor.lnum = mrb_fixnum(lnum);
  win->w_cursor.col = mrb_fixnum(col);
  check_cursor();            /* put cursor on an existing line */

  update_screen(NOT_VALID);

  return mrb_nil_value();
}

static mrb_value f_nop(mrb_state* mrb, mrb_value self)
{
  return mrb_nil_value();
}

static mrb_value f_p(mrb_state* mrb, mrb_value self)
{
  int argc;
  mrb_value *argv;

  mrb_get_args(mrb, "*", &argv, &argc);
  auto str = mrb_str_new(mrb, "", 0);

  for (int i=0; i<argc; i++) 
  {
    if (i > 0) mrb_str_cat(mrb, str, ", ", 2);
    mrb_str_concat(mrb, str, mrb_inspect(mrb, argv[i]));
  }

  MSG(RSTRING_PTR(str));

  return mrb_nil_value();
}

static void mruby_io_init(void)
{
  mrb_define_method(mrb, mrb->kernel_module, "p", f_p, ARGS_OPT(1));
  mrb_define_method(mrb, mrb->kernel_module, "puts", vim_message, ARGS_OPT(1));
}

static void mruby_vim_init(void)
{
  objtbl = mrb_hash_new(mrb);
  mrb_gv_set(mrb, mrb_intern(mrb, "$VIM_OBJECTS"), objtbl);

  mVIM = mrb_define_module(mrb, "Vim");
  mrb_define_const(mrb, mrb->object_class, "VIM", mrb_obj_value(mVIM));
  mrb_define_const(mrb, mVIM, "VERSION_MAJOR", mrb_fixnum_value(VIM_VERSION_MAJOR));
  mrb_define_const(mrb, mVIM, "VERSION_MINOR", mrb_fixnum_value(VIM_VERSION_MINOR));
  mrb_define_const(mrb, mVIM, "VERSION_BUILD", mrb_fixnum_value(VIM_VERSION_BUILD));
  mrb_define_const(mrb, mVIM, "VERSION_PATCHLEVEL", mrb_fixnum_value(VIM_VERSION_PATCHLEVEL));
  mrb_define_const(mrb, mVIM, "VERSION_SHORT", mrb_str_new_cstr(mrb, VIM_VERSION_SHORT));
  mrb_define_const(mrb, mVIM, "VERSION_MEDIUM", mrb_str_new_cstr(mrb, VIM_VERSION_MEDIUM));
  mrb_define_const(mrb, mVIM, "VERSION_LONG", mrb_str_new_cstr(mrb, VIM_VERSION_LONG));
  mrb_define_const(mrb, mVIM, "VERSION_LONG_DATE", mrb_str_new_cstr(mrb, VIM_VERSION_LONG_DATE));
  mrb_define_module_function(mrb, mVIM, "message", vim_message, ARGS_REQ(1));
  mrb_define_module_function(mrb, mVIM, "set_option", vim_set_option, ARGS_REQ(1));
  mrb_define_module_function(mrb, mVIM, "command", vim_command, ARGS_REQ(1));
  mrb_define_module_function(mrb, mVIM, "evaluate", vim_evaluate, ARGS_REQ(1));

  eDeletedBufferError = mrb_define_class_under(mrb, mVIM, "DeletedBufferError",
    mrb->eStandardError_class);
  eDeletedWindowError = mrb_define_class_under(mrb, mVIM, "DeletedWindowError",
    mrb->eStandardError_class);

  cBuffer = mrb_define_class_under(mrb, mVIM, "Buffer", mrb->object_class);
  mrb_define_singleton_method(mrb, cBuffer, "current", buffer_s_current, ARGS_NONE());
  mrb_define_singleton_method(mrb, cBuffer, "count", buffer_s_count, ARGS_NONE());
  mrb_define_singleton_method(mrb, cBuffer, "[]", buffer_s_aref, 1);
  mrb_define_method(mrb, cBuffer, "name", buffer_name, ARGS_NONE());
  mrb_define_method(mrb, cBuffer, "number", buffer_number, ARGS_NONE());
  mrb_define_method(mrb, cBuffer, "count", buffer_count, ARGS_NONE());
  mrb_define_method(mrb, cBuffer, "length", buffer_count, ARGS_NONE());
  mrb_define_method(mrb, cBuffer, "[]", buffer_aref, ARGS_REQ(1));
  mrb_define_method(mrb, cBuffer, "[]=", buffer_aset, ARGS_REQ(2));
  mrb_define_method(mrb, cBuffer, "delete", buffer_delete, ARGS_REQ(1));
  mrb_define_method(mrb, cBuffer, "append", buffer_append, ARGS_REQ(2));

  mrb_define_method(mrb, cBuffer, "line_number", current_line_number, ARGS_NONE());
  mrb_define_method(mrb, cBuffer, "line", line_s_current, ARGS_NONE());
  mrb_define_method(mrb, cBuffer, "line=", set_current_line, ARGS_REQ(1));

  cVimWindow = mrb_define_class_under(mrb, mVIM, "Window", mrb->object_class);
  mrb_define_singleton_method(mrb, cVimWindow, "current", window_s_current, ARGS_NONE());
  mrb_define_singleton_method(mrb, cVimWindow, "count", window_s_count, ARGS_NONE());
  mrb_define_singleton_method(mrb, cVimWindow, "[]", window_s_aref, ARGS_REQ(1));
  mrb_define_method(mrb, cVimWindow, "buffer", window_buffer, ARGS_NONE());
  mrb_define_method(mrb, cVimWindow, "height", window_height, ARGS_NONE());
  mrb_define_method(mrb, cVimWindow, "height=", window_set_height, ARGS_REQ(1));
  mrb_define_method(mrb, cVimWindow, "width", window_width, ARGS_NONE());
  mrb_define_method(mrb, cVimWindow, "width=", window_set_width, ARGS_REQ(1));
  mrb_define_method(mrb, cVimWindow, "cursor", window_cursor, ARGS_NONE());
  mrb_define_method(mrb, cVimWindow, "cursor=", window_set_cursor, ARGS_REQ(1));

  mrb_define_method(mrb, mrb->kernel_module, "__curbuf", buffer_s_current, ARGS_NONE());
  mrb_define_method(mrb, mrb->kernel_module, "__curwin", window_s_current, ARGS_NONE());
}
