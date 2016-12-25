// Copyright (C) 2009-2013 D. V. Wiebe
//
///////////////////////////////////////////////////////////////////////////
//
// This file is part of the GetData project.
//
// GetData is free software; you can redistribute it and/or modify it under
// the terms of the GNU Lesser General Public License as published by the
// Free Software Foundation; either version 2.1 of the License, or (at your
// option) any later version.
//
// GetData is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
// License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with GetData; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//
#ifdef HAVE_CONFIG_H
#include "gd_config.h"
#endif
#undef GETDATA_LEGACY_API

#include "getdata/dirfile.h"
#include "internal.h"

#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <iostream>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef HAVE_DIRECT_H
#include <direct.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if MKDIR_NO_MODE
#ifdef HAVE__MKDIR
#define mkdir(f,m) _mkdir(f)
#else
#define mkdir(f,m) mkdir(f)
#endif
#endif
#ifndef O_BINARY
#define O_BINARY 0
#endif

using namespace std;
using namespace GetData;

static int ne = 0;

template <class T> static void CheckT(char c, int i, int t, int n, T v, T g,
    int r)
{
  if (r) {
    ne++;
    cerr << c;
    if (i != -1)
      cerr << "(" << i << ")";
    cerr << "[" << t;
    if (n != -1)
      cerr << ", " << n;
    cerr << "] = " << v << " (expected " << g << ")" << endl;
  }
}

static void CheckError(const Dirfile *d, int t, int n, int g)
{
  int e = d->Error();
  CheckT<int>('e', -1, t, n, e, g, e != g);
}

static void CheckInt(int i, int t, int n, int v, int g)
{
  CheckT<int>('n', i, t, n, v, g, v != g);
}

template <class T> static void CheckFloat(char c, int i, int t, int n, T v, T g)
{
  CheckT<T>(c, i, t, n, v, g, (abs((v) - (g)) > 1e-10));
}
#define CHECK_ERROR(t,g) CheckError(d,t,-1,g)
#define CHECK_ERROR2(t,n,g) CheckError(d,t,n,g)
#define CHECK_OK(t) CHECK_ERROR(t,GD_E_OK)
#define CHECK_OK2(t,n) CHECK_ERROR2(t,n,GD_E_OK)

#define CHECK_NONNULL(t,v) CheckT<const void*>('p', -1, t, -1, v, NULL, !(v))
#define CHECK_NONNULL2(t,n,v) CheckT<const void*>('p', -1, t, n, v, NULL, !(v))

#define CHECK_NULL(t,v) CheckT<const void*>('p', -1, t, -1, v, NULL, !!(v))
#define CHECK_NULL2(t,n,v) CheckT<const void*>('p', -1, t, n, v, NULL, !!(v))

#define CHECK_INT(t,v,g) CheckInt(-1, t, -1, v, g)
#define CHECK_INT2(t,n,v,g) CheckInt(-1, t, n, v, g)

#define CHECK_INT_ARRAY(t,m,v,g) \
  for (i = 0; i < m; ++i) CheckInt(i, t, -1, v, g)

#define CHECK_DOUBLE(t,v,g) CheckFloat<double>('d', -1, t, -1, v, g)
#define CHECK_DOUBLE2(t,n,v,g) CheckFloat<double>('d', -1, t, n, v, g)

#define CHECK_DOUBLE_ARRAY(t,n,m,v,g) \
  for (i = 0; i < m; ++i) CheckFloat<double>('d', i, t, n, v, g)

#define CHECK_STRINGi(t,i,v,g) \
  CheckT<const char*>('s', i, t, -1, v, g, (strcmp((v), (g))))
#define CHECK_STRING(t,v,g) CHECK_STRINGi(t,-1,v,g)
#define CHECK_STRING2(t,n,v,g) \
  CheckT<const char*>('s', -1, t, n, v, g, (strcmp((v), (g))))

#define CHECK_STRING_ARRAY(t,m,v,g) \
  for (i = 0; i < m; ++i) \
CheckT<const char*>('s', i, t, -1, v, g, (strcmp((v), (g))))

#define CHECK_EOSTRING(t,v,g) \
  CheckT<const char*>('S', -1, t, -1, v, g, \
      (strcmp((v) + strlen(v) - strlen(g), (g))))

#define CHECK_COMPLEX2(t,n,v,g) \
  CheckFloat<complex<double> >('c', -1, t, n, v, g)
#define CHECK_COMPLEX_ARRAY(t,m,v,g) \
  for (i = 0; i < m; ++i) CheckFloat<complex<double> >('c', i, t, -1, v, g)

void run_tests(void)
{
  const char* filedir = "dirfile";
  const char* format = "dirfile/format";
  const char* empty = "dirfile/empty";
  const char* eformat = "dirfile/empty/format";
  const char* format1 = "dirfile/format1";
  const char* format2 = "dirfile/format2";
  const char* form2 = "dirfile/form2";
  const char* new1 = "dirfile/new1";
  const char* data = "dirfile/data";
  const char* format_data =
    "/ENDIAN little\n"
    "data RAW INT8 8\n"
    "lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp const const\n"
    "/META data mstr STRING \"This is a string constant.\"\n"
    "/META data mconst CONST COMPLEX128 3.3;4.4\n"
    "/META data mcarray CARRAY FLOAT64 1.9 2.8 3.7 4.6 5.5\n"
    "/META data mlut LINTERP data ./lut\n"
    "const CONST FLOAT64 5.5\n"
    "carray CARRAY FLOAT64 1.1 2.2 3.3 4.4 5.5 6.6\n"
    "linterp LINTERP data ./lut\n"
    "polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const\n"
    "bit BIT data 3 4\n"
    "sbit SBIT data 5 6\n"
    "mplex MPLEX data sbit 1 10\n"
    "mult MULTIPLY data sbit\n"
    "div DIVIDE mult bit\n"
    "recip RECIP div 6.5;4.3\n"
    "phase PHASE data 11\n"
    "window WINDOW linterp mult LT 4.1\n"
    "/ALIAS alias data\n"
    "string STRING \"Zaphod Beeblebrox\"\n"
    "sarray SARRAY one two three four five six seven\n"
    "data/msarray SARRAY eight nine ten eleven twelve\n"
    "indir INDIR data carray\n"
    "sindir SINDIR data sarray\n";
  const char* form2_data = "const2 CONST INT8 -19\n";
  const int nfields = 20;
  unsigned char c[8];
  unsigned char data_data[80];
  signed char sc;
  int m, n, i, j;
  float fl;
  double dp, p[6], q[6];
  const double *qp;
  complex<double> cq[6];
  const char **list;
  const char* str;
  char* tok;
  char buf[GD_MAX_LINE_LENGTH];
  Dirfile *d;
  Entry *ent;
  RawEntry rent, *rep;
  LincomEntry lent, *lep;
  LinterpEntry nent, *nep;
  BitEntry bent, *bep;
  MultiplyEntry ment, *mep;
  PhaseEntry pent, *pep;
  PolynomEntry yent, *yep;
  DivideEntry dent, *dep;
  RecipEntry oent, *oep;
  SBitEntry sent, *sep;
  ConstEntry cent, *cep;
  CarrayEntry aent, *aep;
  StringEntry gent;
  WindowEntry went, *wep;
  MplexEntry xent, *xep;
  SarrayEntry saent, *saep;
  IndirEntry ient, *iep;
  SindirEntry sient, *siep;
  Fragment *frag;
  gd_triplet_t thresh;
  const gd_carray_t *carrays;
  const char*** sarrays;

  char* fields[nfields + 8] = { (char*)"bit", (char*)"div", (char*)"data",
    (char*)"mult", (char*)"sbit", (char*)"INDEX", (char*)"alias",
    (char*)"const", (char*)"indir", (char*)"mplex", (char*)"phase",
    (char*)"recip", (char*)"carray", (char*)"lincom", (char*)"sarray",
    (char*)"sindir", (char*)"string", (char*)"window", (char*)"linterp",
    (char*)"polynom", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
  char *strings[3];

  unlink(data);
  unlink(new1);
  unlink(format);
  unlink(format1);
  unlink(format2);
  unlink(form2);
  rmdir(filedir);

  // Write the test dirfile
  mkdir(filedir, 0777);

  for (n = 0; n < 80; ++n)
    data_data[n] = (unsigned char)n + 1;

  n = open(format, O_CREAT | O_TRUNC | O_WRONLY | O_BINARY, 0666);
  write(n, format_data, strlen(format_data));
  close(n);

  n = open(form2, O_CREAT | O_TRUNC | O_WRONLY | O_BINARY, 0666);
  write(n, form2_data, strlen(form2_data));
  close(n);

  n = open(data, O_CREAT | O_TRUNC | O_WRONLY | O_BINARY, 0666);
  write(n, data_data, 80);
  close(n);

  // 1: Dirfile::Error check
  d = new Dirfile("x");
  CHECK_ERROR(1, GD_E_IO);
  delete d;

  // 2: Dirfile::Dirfile check
  d = new Dirfile(filedir, GD_RDWR);
  CHECK_OK(2);

  // 3: Dirfile::GetData check
  n = d->GetData("data", 5, 0, 1, 0, UInt8, c);
  CHECK_OK(3);
  CHECK_INT(3,n,8);
  CHECK_INT_ARRAY(3,8,c[i],41 + i);

  // 12: Dirfile::GetConstant check
  n = d->GetConstant("const", Float64, &dp);
  CHECK_OK(12);
  CHECK_INT(12,n,0);
  CHECK_DOUBLE(12,dp,5.5);

  // 23: Dirfile::NFields check
  n = d->NFields();
  CHECK_OK(23);
  CHECK_INT(23,n,nfields);

  // 25: Dirfile::FieldList check
  list = d->FieldList();
  CHECK_OK(25);
  CHECK_STRING_ARRAY(25,n,list[i],fields[i]);

  // 26: Dirfile::NFields check
  n = d->NMFields("data");
  CHECK_OK(26);
  CHECK_INT(26,n,5);

  // 27: Dirfile::MFieldList check
  fields[0] = (char*)"mstr";
  fields[1] = (char*)"mconst";
  fields[2] = (char*)"mcarray";
  fields[3] = (char*)"mlut";
  fields[4] = (char*)"msarray";
  list = d->MFieldList("data");
  CHECK_OK(27);
  CHECK_STRING_ARRAY(27,n,list[i],fields[i]);

  // 28: Dirfile::NFrames check
  n = d->NFrames();
  CHECK_OK(28);
  CHECK_INT(28,n,10);

  // 29: Dirfile::SamplesPerFrame check
  n = d->SamplesPerFrame("data");
  CHECK_OK(29);
  CHECK_INT(29,n,8);

  // 30: Dirfile::PutData check
  c[0] = 13;
  c[1] = 14;
  c[2] = 15;
  c[3] = 16;
  n = d->PutData("data", 5, 1, 0, 4, UInt8, c);
  CHECK_OK2(30,1);
  CHECK_INT2(30,1,n,4);

  n = d->GetData("data", 5, 0, 1, 0, UInt8, c);
  CHECK_OK2(30,2);
  CHECK_INT2(30,2,n,8);
  CHECK_INT_ARRAY(30,8,c[i],(i == 0 || i > 4) ? 41 + i : 12 + i);

  // 38: Dirfile::ErrorString check
  d->GetData("x", 5, 0, 1, 0, Null, NULL);
  str = d->ErrorString();
  CHECK_STRING(38,str,"Field not found: x");

  // 40: Dirfile::Entry / RawEntry check
  ent = d->Entry("data");
  CHECK_OK(40);
  CHECK_INT2(40,1,ent->Type(),RawEntryType);
  CHECK_INT2(40,2,ent->FragmentIndex(),0);
  CHECK_INT2(40,3,ent->SamplesPerFrame(),8);
  CHECK_INT2(40,4,ent->RawType(),Int8);
  delete ent;

  // 42: Dirfile::Entry / LincomEntry check
  cq[0] = 1.1;
  cq[1] = 2.2;
  cq[2] = 2.2;
  cq[3] = complex<double>(3.3, 4.4);
  cq[4] = 5.5;
  cq[5] = 5.5;
  ent = d->Entry("lincom");
  CHECK_OK(42);
  CHECK_INT2(42,1,ent->Type(),LincomEntryType);
  CHECK_INT2(42,2,ent->NFields(),3);
  CHECK_INT2(42,3,ent->FragmentIndex(),0);
  CHECK_STRING2(42,4,ent->Input(0),"data");
  CHECK_STRING2(42,5,ent->Input(1),"INDEX");
  CHECK_STRING2(42,6,ent->Input(2),"linterp");
  CHECK_INT2(42,7,ent->ComplexScalars(),1);
  CHECK_COMPLEX_ARRAY(42,3,ent->CScale(i),cq[i * 2]);
  CHECK_COMPLEX_ARRAY(42,3,ent->COffset(i),cq[i * 2 + 1]);
  delete ent;

  // 44: Dirfile::Entry / PolynomEntry check
  ent = d->Entry("polynom");
  CHECK_OK(44);
  CHECK_INT2(44,1,ent->Type(),PolynomEntryType);
  CHECK_INT2(44,2,ent->PolyOrd(),5);
  CHECK_INT2(44,3,ent->FragmentIndex(),0);
  CHECK_STRING2(44,4,ent->Input(0),"data");
  CHECK_INT2(44,7,ent->ComplexScalars(),1);
  CHECK_COMPLEX_ARRAY(44,6,ent->CCoefficient(i),cq[i]);
  delete ent;

  // 45: Dirfile::Entry / LinterpEntry check
  ent = d->Entry("linterp");
  CHECK_OK(45);
  CHECK_INT2(45,1,ent->Type(),LinterpEntryType);
  CHECK_INT2(45,2,ent->FragmentIndex(),0);
  CHECK_STRING2(45,3,ent->Input(0),"data");
  CHECK_STRING2(45,4,ent->Table(),"./lut");
  delete ent;

  // 46: Dirfile::Entry / BitEntry check
  ent = d->Entry("bit");
  CHECK_OK(46);
  CHECK_INT2(46,1,ent->Type(),BitEntryType);
  CHECK_INT2(46,2,ent->FragmentIndex(),0);
  CHECK_STRING2(46,3,ent->Input(0),"data");
  CHECK_INT2(46,4,ent->NumBits(),4);
  CHECK_INT2(46,5,ent->FirstBit(),3);
  delete ent;

  // 47: Dirfile::Entry / SBitEntry check
  ent = d->Entry("sbit");
  CHECK_OK(47);
  CHECK_INT2(47,1,ent->Type(),SBitEntryType);
  CHECK_INT2(47,2,ent->FragmentIndex(),0);
  CHECK_STRING2(47,3,ent->Input(0),"data");
  CHECK_INT2(47,4,ent->NumBits(),6);
  CHECK_INT2(47,5,ent->FirstBit(),5);
  delete ent;

  // 48: Dirfile::Entry / MultiplyEntry check
  ent = d->Entry("mult");
  CHECK_OK(48);
  CHECK_INT2(48,1,ent->Type(),MultiplyEntryType);
  CHECK_INT2(48,2,ent->FragmentIndex(),0);
  CHECK_STRING2(48,3,ent->Input(0),"data");
  CHECK_STRING2(48,4,ent->Input(1),"sbit");
  delete ent;

  // 49: Dirfile::Entry / PhaseEntry check
  ent = d->Entry("phase");
  CHECK_OK(49);
  CHECK_INT2(49,1,ent->Type(),PhaseEntryType);
  CHECK_INT2(49,2,ent->FragmentIndex(),0);
  CHECK_STRING2(49,3,ent->Input(0),"data");
  CHECK_INT2(49,4,ent->Shift(),11);
  delete ent;

  // 50: Dirfile::Entry / ConstEntry check
  ent = d->Entry("const");
  CHECK_OK(50);
  CHECK_INT2(50,1,ent->Type(),ConstEntryType);
  CHECK_INT2(50,2,ent->FragmentIndex(),0);
  CHECK_INT2(50,3,ent->ConstType(),Float64);
  delete ent;

  // 51: Dirfile::Entry / StringEntry check
  ent = d->Entry("string");
  CHECK_OK(51);
  CHECK_INT2(51,1,ent->Type(),StringEntryType);
  CHECK_INT2(51,2,ent->FragmentIndex(),0);
  delete ent;

  // 52: Dirfile::FragmentIndex check
  n = d->FragmentIndex("data");
  CHECK_OK(52);
  CHECK_INT(52,n,0);

  // 53: Dirfile::Add / RawEntry check
  rent.SetName("new1");
  rent.SetFragmentIndex(0);
  rent.SetSamplesPerFrame(3);
  rent.SetType(Float64);
  d->Add(rent);
  CHECK_OK2(53,1);

  ent = d->Entry("new1");
  CHECK_OK2(53,2);
  CHECK_INT2(53,1,ent->Type(),RawEntryType);
  CHECK_INT2(53,2,ent->FragmentIndex(),0);
  CHECK_INT2(53,3,ent->SamplesPerFrame(),3);
  CHECK_INT2(53,4,ent->RawType(),Float64);
  delete ent;

  // 54: Dirfile::Add / LincomEntry check
  q[0] = 9.9;
  q[1] = 8.8;
  q[2] = 7.7;
  q[3] = 6.6;
  lent.SetName("new2");
  lent.SetFragmentIndex(0);
  lent.SetNFields(2);
  lent.SetInput("in1", 0);
  lent.SetScale(q[0], 0);
  lent.SetOffset(q[1], 0);
  lent.SetInput("in2", 1);
  lent.SetScale(q[2], 1);
  lent.SetOffset(q[3], 1);
  d->Add(lent);
  CHECK_OK2(54,1);

  ent = d->Entry("new2");
  CHECK_OK2(54,2);
  CHECK_INT2(54,1,ent->Type(),LincomEntryType);
  CHECK_INT2(54,2,ent->NFields(),2);
  CHECK_INT2(54,3,ent->FragmentIndex(),0);
  CHECK_STRING2(54,4,ent->Input(0),"in1");
  CHECK_STRING2(54,5,ent->Input(1),"in2");
  CHECK_INT2(54,6,ent->ComplexScalars(),0);
  CHECK_DOUBLE_ARRAY(54,7,2,ent->Scale(i),q[i * 2]);
  CHECK_DOUBLE_ARRAY(54,8,2,ent->Offset(i),q[i * 2 + 1]);
  delete ent;

  // 55: Dirfile::Add / LincomEntry check
  cq[0] = complex<double>(1.1, 1.2);
  cq[1] = complex<double>(1.3, 1.4);
  cq[2] = complex<double>(1.4, 1.5);
  cq[3] = complex<double>(1.6, 1.7);
  lent.Dissociate();
  lent.SetName("new3");
  lent.SetFragmentIndex(0);
  lent.SetNFields(2);
  lent.SetInput("in1", 0);
  lent.SetScale(cq[0], 0);
  lent.SetOffset(cq[1], 0);
  lent.SetInput("in2", 1);
  lent.SetScale(cq[2], 1);
  lent.SetOffset(cq[3], 1);
  d->Add(lent);
  CHECK_OK2(55,1);

  ent = d->Entry("new3");
  CHECK_OK2(55,2);
  CHECK_INT2(55,1,ent->Type(),LincomEntryType);
  CHECK_INT2(55,2,ent->NFields(),2);
  CHECK_INT2(55,3,ent->FragmentIndex(),0);
  CHECK_STRING2(55,4,ent->Input(0),"in1");
  CHECK_STRING2(55,5,ent->Input(1),"in2");
  CHECK_INT2(55,6,ent->ComplexScalars(),1);
  CHECK_COMPLEX_ARRAY(55,2,ent->CScale(i),cq[i * 2]);
  CHECK_COMPLEX_ARRAY(55,2,ent->COffset(i),cq[i * 2 + 1]);
  delete ent;

  // 56: Dirfile::Add / PolynomEntry check
  q[0] = 3.9;
  q[1] = 4.8;
  q[2] = 5.7;
  q[3] = 6.6;
  yent.SetName("new4");
  yent.SetFragmentIndex(0);
  yent.SetPolyOrd(3);
  yent.SetInput("in1");
  yent.SetCoefficient(q[0], 0);
  yent.SetCoefficient(q[1], 1);
  yent.SetCoefficient(q[2], 2);
  yent.SetCoefficient(q[3], 3);
  d->Add(yent);
  CHECK_OK2(56,1);

  ent = d->Entry("new4");
  CHECK_OK2(56,2);
  CHECK_INT2(56,1,ent->Type(),PolynomEntryType);
  CHECK_INT2(56,2,ent->PolyOrd(),3);
  CHECK_INT2(56,3,ent->FragmentIndex(),0);
  CHECK_STRING2(56,4,ent->Input(0),"in1");
  CHECK_INT2(56,5,ent->ComplexScalars(),0);
  CHECK_DOUBLE_ARRAY(56,6,4,ent->Coefficient(i),q[i]);
  delete ent;

  // 57: Dirfile::Add / PolynomEntry check
  cq[0] = complex<double>(3.1, 7);
  cq[1] = complex<double>(4.2, 8);
  cq[2] = complex<double>(5.2, 9);
  cq[3] = complex<double>(6.3, 4.4);
  yent.Dissociate();
  yent.SetName("new5");
  yent.SetFragmentIndex(0);
  yent.SetPolyOrd(3);
  yent.SetInput("in2");
  yent.SetCoefficient(cq[0], 0);
  yent.SetCoefficient(cq[1], 1);
  yent.SetCoefficient(cq[2], 2);
  yent.SetCoefficient(cq[3], 3);
  d->Add(yent);
  CHECK_OK2(57,1);

  ent = d->Entry("new5");
  CHECK_OK2(57,2);
  CHECK_INT2(57,1,ent->Type(),PolynomEntryType);
  CHECK_INT2(57,2,ent->PolyOrd(),3);
  CHECK_INT2(57,3,ent->FragmentIndex(),0);
  CHECK_STRING2(57,4,ent->Input(0),"in2");
  CHECK_INT2(57,7,ent->ComplexScalars(),1);
  CHECK_COMPLEX_ARRAY(57,4,ent->CCoefficient(i),cq[i]);
  delete ent;

  // 58: Dirfile::Add / LinterpEntry check
  nent.SetName("new6");
  nent.SetFragmentIndex(0);
  nent.SetInput("in");
  nent.SetTable("./some/table");
  d->Add(nent);
  CHECK_OK2(58,1);

  ent = d->Entry("new6");
  CHECK_OK2(58,2);
  CHECK_INT2(58,1,ent->Type(),LinterpEntryType);
  CHECK_INT2(58,2,ent->FragmentIndex(),0);
  CHECK_STRING2(58,3,ent->Input(0),"in");
  CHECK_STRING2(58,4,ent->Table(),"./some/table");
  delete ent;

  // 59: Dirfile::Add / BitEntry check
  bent.SetName("new7");
  bent.SetFragmentIndex(0);
  bent.SetInput("in1");
  bent.SetFirstBit(13);
  bent.SetNumBits(12);
  d->Add(bent);
  CHECK_OK2(59,1);

  ent = d->Entry("new7");
  CHECK_OK(59);
  CHECK_INT2(59,1,ent->Type(),BitEntryType);
  CHECK_INT2(59,2,ent->FragmentIndex(),0);
  CHECK_STRING2(59,3,ent->Input(0),"in1");
  CHECK_INT2(59,4,ent->NumBits(),12);
  CHECK_INT2(59,5,ent->FirstBit(),13);
  delete ent;

  // 60: Dirfile::Add / SBitEntry check
  sent.SetName("new8");
  sent.SetFragmentIndex(0);
  sent.SetInput("in2");
  sent.SetFirstBit(14);
  sent.SetNumBits(15);
  d->Add(sent);
  CHECK_OK2(60,1);

  ent = d->Entry("new8");
  CHECK_OK(60);
  CHECK_INT2(60,1,ent->Type(),SBitEntryType);
  CHECK_INT2(60,2,ent->FragmentIndex(),0);
  CHECK_STRING2(60,3,ent->Input(0),"in2");
  CHECK_INT2(60,4,ent->NumBits(),15);
  CHECK_INT2(60,5,ent->FirstBit(),14);
  delete ent;

  // 61: Dirfile::Add / MultiplyEntry check
  ment.SetName("new9");
  ment.SetFragmentIndex(0);
  ment.SetInput("in1", 0);
  ment.SetInput("in2", 1);
  d->Add(ment);
  CHECK_OK2(61,1);

  ent = d->Entry("new9");
  CHECK_OK2(61,2);
  CHECK_INT2(61,1,ent->Type(),MultiplyEntryType);
  CHECK_INT2(61,2,ent->FragmentIndex(),0);
  CHECK_STRING2(61,3,ent->Input(0),"in1");
  CHECK_STRING2(61,4,ent->Input(1),"in2");
  delete ent;

  // 62: Dirfile::Add / PhaseEntry check
  pent.SetName("new10");
  pent.SetFragmentIndex(0);
  pent.SetInput("in1");
  pent.SetShift(22);
  d->Add(pent);
  CHECK_OK2(62,1);

  ent = d->Entry("new10");
  CHECK_OK(62);
  CHECK_INT2(62,1,ent->Type(),PhaseEntryType);
  CHECK_INT2(62,2,ent->FragmentIndex(),0);
  CHECK_STRING2(62,3,ent->Input(0),"in1");
  CHECK_INT2(62,4,ent->Shift(),22);
  delete ent;

  // 63: Dirfile::Add / ConstEntry check
  cent.SetName("new11");
  cent.SetFragmentIndex(0);
  cent.SetType(Float64);
  d->Add(cent);
  CHECK_OK2(63,1);

  ent = d->Entry("new11");
  CHECK_OK2(63,2);
  CHECK_INT2(63,1,ent->Type(),ConstEntryType);
  CHECK_INT2(63,2,ent->FragmentIndex(),0);
  CHECK_INT2(63,3,ent->ConstType(),Float64);
  delete ent;

  // 64: Fragment check
  frag = d->Fragment(0);
  CHECK_OK(64);
  sprintf(buf, "dirfile%cformat", GD_DIRSEP);
  CHECK_EOSTRING(64,frag->Name(), buf);
  delete frag;

  // 65: Dirfile::NFragments check
  n = d->NFragments();
  CHECK_OK(65);
  CHECK_INT(65,n,1);

  // 66: Dirfile::Include check
  n = d->Include("form2");
  CHECK_OK2(66,1);
  CHECK_INT2(66,1,n,1);
  n = d->GetConstant("const2", Int8, &sc);
  CHECK_OK2(66,2);
  CHECK_INT2(66,2,sc,-19);

  // 67: Dirfile::NFieldsByType check
  n = d->NFieldsByType(LincomEntryType);
  CHECK_OK(67);
  CHECK_INT(67,n,3);

  // 68: Dirfile::FieldListByType check
  fields[0] = (char*)"new2";
  fields[1] = (char*)"new3";
  fields[2] = (char*)"lincom";
  list = d->FieldListByType(LincomEntryType);
  CHECK_OK(68);
  CHECK_STRING_ARRAY(68,n,list[i],fields[i]);

  // 69: Dirfile::NVectors check
  n = d->NVectors();
  CHECK_OK(69);
  CHECK_INT(69,n,25);

  // 70: Dirfile::VectorList check
  fields[ 0] = (char*)"bit";
  fields[ 1] = (char*)"div";
  fields[ 2] = (char*)"data";
  fields[ 3] = (char*)"mult";
  fields[ 4] = (char*)"new1";
  fields[ 5] = (char*)"new2";
  fields[ 6] = (char*)"new3";
  fields[ 7] = (char*)"new4";
  fields[ 8] = (char*)"new5";
  fields[ 9] = (char*)"new6";
  fields[10] = (char*)"new7";
  fields[11] = (char*)"new8";
  fields[12] = (char*)"new9";
  fields[13] = (char*)"sbit";
  fields[14] = (char*)"INDEX";
  fields[15] = (char*)"alias";
  fields[16] = (char*)"indir";
  fields[17] = (char*)"mplex";
  fields[18] = (char*)"new10";
  fields[19] = (char*)"phase";
  fields[20] = (char*)"recip";
  fields[21] = (char*)"lincom";
  fields[22] = (char*)"window";
  fields[23] = (char*)"linterp";
  fields[24] = (char*)"polynom";
  list = d->VectorList();
  CHECK_OK(70);
  CHECK_STRING_ARRAY(70,n,list[i],fields[i]);

  // 81: Dirfile::GetString check
  n = d->GetString("string", GD_MAX_LINE_LENGTH, buf);
  CHECK_OK(81);
  CHECK_INT(81,n,18);
  CHECK_STRING(81,buf,"Zaphod Beeblebrox");

  // 82: Dirfile::Add / StringEntry check
  gent.SetName("new12");
  gent.SetFragmentIndex(0);
  d->Add(gent);
  CHECK_OK2(82,1);

  ent = d->Entry("new12");
  CHECK_OK2(82,2);
  CHECK_INT2(82,1,ent->Type(),StringEntryType);
  CHECK_INT2(82,2,ent->FragmentIndex(),0);
  delete ent;

  n = d->GetString("new12", GD_MAX_LINE_LENGTH, buf);
  CHECK_OK2(82,3);
  CHECK_INT(82,n,1);
  CHECK_STRING(82,buf,"");

  // 84: Dirfile::AddSpec check
  d->AddSpec("lorem STRING \"Lorem ipsum\"", 0);
  CHECK_OK2(84,1);

  n = d->GetString("lorem", GD_MAX_LINE_LENGTH, buf);
  CHECK_OK2(84,2);
  CHECK_INT(84,n,12);
  CHECK_STRING(84,buf,"Lorem ipsum");

  // 85: Dirfile::MAddSpec check
  d->MAddSpec("ipsum STRING \"dolor sit amet.\"", "lorem");
  CHECK_OK2(85,1);

  n = d->GetString("lorem/ipsum", GD_MAX_LINE_LENGTH, buf);
  CHECK_OK2(85,2);
  CHECK_INT(85,n,16);
  CHECK_STRING(85,buf,"dolor sit amet.");

  // 86: Dirfile::PutConstant check
  sc = 86;
  n = d->PutConstant("const", Int8, &sc);
  CHECK_OK2(86,1);
  CHECK_INT2(86,1,n,0);

  n = d->GetConstant("const", Float32, &fl);
  CHECK_OK2(86,2);
  CHECK_INT2(86,2,n,0);
  CHECK_DOUBLE2(86,3,fl,86);

  // 94: Dirfile::PutString check
  n = d->PutString("string", "Arthur Dent");
  CHECK_OK2(94,1);
  CHECK_INT2(94,1,n,0);

  n = d->GetString("string", GD_MAX_LINE_LENGTH, buf);
  CHECK_OK2(94,2);
  CHECK_INT2(94,2,n,12);
  CHECK_STRING(94,buf,"Arthur Dent");

  // 95: Dirfile::NMFieldsByType check
  n = d->NMFieldsByType("data", LinterpEntryType);
  CHECK_OK(95);
  CHECK_INT(95,n,1);

  // 96: Dirfile::MFieldListByType check
  fields[0] = (char*)"mlut";
  list = d->MFieldListByType("data", LinterpEntryType);
  CHECK_OK(96);
  CHECK_STRING_ARRAY(96,n,list[i],fields[i]);

  // 97: Dirfile::NMVectors check
  n = d->NMVectors("data");
  CHECK_OK(97);
  CHECK_INT(97,n,1);

  // 98: Dirfile::MVectorList check
  fields[0] = (char*)"mlut";
  list = d->MVectorList("data");
  CHECK_OK(98);
  CHECK_STRING_ARRAY(98,n,list[i],fields[i]);

  // 99: RawEntry check
  rep = reinterpret_cast<RawEntry*>(d->Entry("new1"));
  CHECK_OK2(99,1);
  rep->SetType(Int32,0);
  CHECK_OK2(99,2);
  rep->SetSamplesPerFrame(4,0);
  CHECK_OK2(99,3);

  ent = d->Entry("new1");
  CHECK_OK2(99,4);
  CHECK_INT2(99,1,ent->Type(),RawEntryType);
  CHECK_INT2(99,2,ent->FragmentIndex(),0);
  CHECK_INT2(99,3,ent->SamplesPerFrame(),4);
  CHECK_INT2(99,4,ent->RawType(),Int32);
  delete ent;

  // 100: LincomEntry check
  lep = reinterpret_cast<LincomEntry*>(d->Entry("new2"));
  CHECK_OK2(100,1);
  lep->SetNFields(3);
  CHECK_OK2(100,2);
  lep->SetInput("in4",2);
  CHECK_OK2(100,3);
  lep->SetScale(1.96,2);
  CHECK_OK2(100,4);
  lep->SetOffset(0.22,2);
  CHECK_OK2(100,5);
  delete lep;

  q[0] = 9.9;
  q[1] = 8.8;
  q[2] = 7.7;
  q[3] = 6.6;
  q[4] = 1.96;
  q[5] = 0.22;
  ent = d->Entry("new2");
  CHECK_OK2(100,6);
  CHECK_INT2(100,1,ent->Type(),LincomEntryType);
  CHECK_INT2(100,2,ent->NFields(),3);
  CHECK_INT2(100,3,ent->FragmentIndex(),0);
  CHECK_STRING2(100,4,ent->Input(0),"in1");
  CHECK_STRING2(100,5,ent->Input(1),"in2");
  CHECK_STRING2(100,6,ent->Input(2),"in4");
  CHECK_INT2(100,7,ent->ComplexScalars(),0);
  CHECK_DOUBLE_ARRAY(100,8,3,ent->Scale(i),q[i * 2]);
  CHECK_DOUBLE_ARRAY(100,9,3,ent->Offset(i),q[i * 2 + 1]);
  delete ent;

  // 102: PolynomEntry check
  yep = reinterpret_cast<PolynomEntry*>(d->Entry("new4"));
  CHECK_OK2(102,1);
  yep->SetInput("in4");
  CHECK_OK2(102,2);
  yep->SetPolyOrd(4);
  CHECK_OK2(102,3);
  yep->SetCoefficient(55.5,4);
  CHECK_OK2(102,4);
  delete yep;

  q[0] = 3.9;
  q[1] = 4.8;
  q[2] = 5.7;
  q[3] = 6.6;
  q[4] = 55.5;
  ent = d->Entry("new4");
  CHECK_OK2(102,5);
  CHECK_INT2(102,1,ent->Type(),PolynomEntryType);
  CHECK_INT2(102,2,ent->PolyOrd(),4);
  CHECK_INT2(102,3,ent->FragmentIndex(),0);
  CHECK_STRING2(102,4,ent->Input(0),"in4");
  CHECK_INT2(102,5,ent->ComplexScalars(),0);
  CHECK_DOUBLE_ARRAY(102,6,5,ent->Coefficient(i),q[i]);
  delete ent;

  // 104: LinterpEntry check
  nep = reinterpret_cast<LinterpEntry*>(d->Entry("new6"));
  CHECK_OK2(104,1);
  nep->SetInput("in3");
  CHECK_OK2(104,2);
  nep->SetTable("./other/table");
  CHECK_OK2(104,3);
  delete nep;

  ent = d->Entry("new6");
  CHECK_OK2(104,2);
  CHECK_INT2(104,1,ent->Type(),LinterpEntryType);
  CHECK_INT2(104,2,ent->FragmentIndex(),0);
  CHECK_STRING2(104,3,ent->Input(0),"in3");
  CHECK_STRING2(104,4,ent->Table(),"./other/table");
  delete ent;

  // 105: BitEntry check
  bep = reinterpret_cast<BitEntry*>(d->Entry("new7"));
  CHECK_OK2(105,1);
  bep->SetInput("in3");
  CHECK_OK2(105,2);
  bep->SetFirstBit(3);
  CHECK_OK2(105,3);
  bep->SetNumBits(2);
  CHECK_OK2(105,4);
  delete bep;

  ent = d->Entry("new7");
  CHECK_OK(105);
  CHECK_INT2(105,1,ent->Type(),BitEntryType);
  CHECK_INT2(105,2,ent->FragmentIndex(),0);
  CHECK_STRING2(105,3,ent->Input(0),"in3");
  CHECK_INT2(105,4,ent->NumBits(),2);
  CHECK_INT2(105,5,ent->FirstBit(),3);
  delete ent;

  // 106: SBitEntry check
  sep = reinterpret_cast<SBitEntry*>(d->Entry("new8"));
  CHECK_OK2(106,1);
  sep->SetInput("in4");
  CHECK_OK2(106,2);
  sep->SetFirstBit(1);
  CHECK_OK2(106,3);
  sep->SetNumBits(22);
  CHECK_OK2(106,4);
  delete sep;

  ent = d->Entry("new8");
  CHECK_OK(106);
  CHECK_INT2(106,1,ent->Type(),SBitEntryType);
  CHECK_INT2(106,2,ent->FragmentIndex(),0);
  CHECK_STRING2(106,3,ent->Input(0),"in4");
  CHECK_INT2(106,4,ent->NumBits(),22);
  CHECK_INT2(106,5,ent->FirstBit(),1);
  delete ent;

  // 107: MultiplyEntry check
  mep = reinterpret_cast<MultiplyEntry*>(d->Entry("new9"));
  CHECK_OK2(107,1);
  mep->SetInput("in4",0);
  CHECK_OK2(107,2);
  mep->SetInput("in5",1);
  CHECK_OK2(107,3);
  delete mep;

  ent = d->Entry("new9");
  CHECK_OK2(107,2);
  CHECK_INT2(107,1,ent->Type(),MultiplyEntryType);
  CHECK_INT2(107,2,ent->FragmentIndex(),0);
  CHECK_STRING2(107,3,ent->Input(0),"in4");
  CHECK_STRING2(107,4,ent->Input(1),"in5");
  delete ent;

  // 108: PhsaeEntry check
  pep = reinterpret_cast<PhaseEntry*>(d->Entry("new10"));
  CHECK_OK2(108,1);
  pep->SetInput("in2");
  CHECK_OK2(108,2);
  pep->SetShift(8);
  CHECK_OK2(108,3);
  delete pep;

  ent = d->Entry("new10");
  CHECK_OK(108);
  CHECK_INT2(108,1,ent->Type(),PhaseEntryType);
  CHECK_INT2(108,2,ent->FragmentIndex(),0);
  CHECK_STRING2(108,3,ent->Input(0),"in2");
  CHECK_INT2(108,4,ent->Shift(),8);
  delete ent;

  // 109: ConstEntry check
  cep = reinterpret_cast<ConstEntry*>(d->Entry("new11"));
  CHECK_OK2(109,1);
  cep->SetType(Float32);
  CHECK_OK2(109,2);
  delete cep;

  ent = d->Entry("new11");
  CHECK_OK2(109,2);
  CHECK_INT2(109,1,ent->Type(),ConstEntryType);
  CHECK_INT2(109,2,ent->FragmentIndex(),0);
  CHECK_INT2(109,3,ent->ConstType(),Float32);
  delete ent;

  // 110: Fragment::Encoding check
  frag = d->Fragment(0);
  CHECK_OK(110);
  CHECK_INT(110,frag->Encoding(),RawEncoding);

  // 111: Fragment::Endianness check
  CHECK_INT(111,frag->Endianness(),GD_LITTLE_ENDIAN | GD_NOT_ARM_ENDIAN);
  delete frag;

  // 112: Dirfile::Name check
  str = d->Name();
  CHECK_OK(112);
  CHECK_EOSTRING(112,str,"dirfile");

  // 113: Fragment::Parent check
  frag = d->Fragment(1);
  CHECK_OK(113);
  CHECK_INT(113,frag->Parent(),0);

  // 114: Fragment::SetProtection check
  frag->SetProtection(GD_PROTECT_DATA);
  CHECK_OK(114);
  delete frag;

  // 115: Fragment::Protection check
  frag = d->Fragment(1);
  CHECK_OK(115);
  CHECK_INT(115,frag->Protection(),GD_PROTECT_DATA);

  // 116: RawEntry::FileName check
  str = rep->FileName();
  CHECK_OK(116);
  sprintf(buf, "dirfile%cnew1", GD_DIRSEP);
  CHECK_EOSTRING(116,str, buf);
  delete rep;

  // 117: Dirfile::Reference check
  rep = d->Reference("new1");
  CHECK_OK(117);
  CHECK_STRING(117,rep->Name(),"new1");
  delete rep;

  // 118: Dirfile::EoF check
  n = d->EoF("lincom");
  CHECK_OK(118);
  CHECK_INT(118,n,80);
  
  // 119: Fragment::SetEncoding check
  frag->SetEncoding(SlimEncoding,0);
  CHECK_OK(119);
  CHECK_INT(119,frag->Encoding(),SlimEncoding);

  // 120: Fragment::SetEndianness check
  frag->SetEndianness(GD_BIG_ENDIAN,0);
  CHECK_OK(120);
  CHECK_INT(120,frag->Endianness(),GD_BIG_ENDIAN);
  delete frag;

  // 121: Dirfile::AlterSpec check
  d->AlterSpec("new10 PHASE in1 3");
  CHECK_OK2(121,1);

  ent = d->Entry("new10");
  CHECK_OK2(121,2);
  CHECK_INT2(121,1,ent->Type(),PhaseEntryType);
  CHECK_INT2(121,2,ent->FragmentIndex(),0);
  CHECK_STRING2(121,3,ent->Input(0),"in1");
  CHECK_INT2(121,4,ent->Shift(),3);
  delete ent;

  //  122: Dirfile::Delete check
  d->Delete("new10", 0);
  CHECK_OK2(122,1);

  ent = d->Entry("new10");
  CHECK_ERROR2(122,2,GD_E_BAD_CODE);
  delete ent;

  // 123: Dirfile::MAlterSpec check
  d->MAlterSpec("mlut LINTERP data /new/lut", "data", 0);
  CHECK_OK2(123,1);

  ent = d->Entry("data/mlut");
  CHECK_OK2(123,2);
  CHECK_INT2(123,3,ent->Type(),LinterpEntryType);
  CHECK_INT2(123,4,ent->FragmentIndex(),0);
  CHECK_STRING2(123,5,ent->Input(0),"data");
  CHECK_STRING2(123,6,ent->Table(),"/new/lut");
  delete ent;

  // 124: Entry::Move check
  ent = d->Entry("new9");
  CHECK_OK2(124,1);
  ent->Move(1,0);
  CHECK_OK2(124,2);
  CHECK_INT(124,ent->FragmentIndex(),1);

  // 125: Entry::Rename check
  ent->Rename("newer",0);
  CHECK_OK2(125,1);
  delete ent;

  ent = d->Entry("new9");
  CHECK_ERROR2(125,2,GD_E_BAD_CODE);
  delete ent;

  ent = d->Entry("newer");
  CHECK_OK2(125,3);
  CHECK_INT2(125,1,ent->Type(),MultiplyEntryType);
  CHECK_INT2(125,2,ent->FragmentIndex(),1);
  CHECK_STRING2(125,3,ent->Input(0),"in4");
  CHECK_STRING2(125,4,ent->Input(1),"in5");
  delete ent;

  // 126: Dirfile::UnInclude check
  d->UnInclude(1,0);
  CHECK_OK2(126,1);

  ent = d->Entry("newer");
  CHECK_ERROR2(126,2,GD_E_BAD_CODE);
  delete ent;

  // 127: Fragment::FrameOffset check
  frag = d->Fragment(0);
  CHECK_OK(127);
  CHECK_INT(127,frag->FrameOffset(),0);

  // 128: Fragment::SetFrameOffset check
  frag->SetFrameOffset(33,0);
  CHECK_OK(128);
  CHECK_INT(128,frag->FrameOffset(),33);

  // 129: Dirfile::NativeType check
  n = d->NativeType("data");
  CHECK_OK(129);
  CHECK_INT(129,n,Int8);

  // 131: Dirfile::Validate check
  n = d->Validate("new7");
  CHECK_ERROR(131,GD_E_BAD_CODE);
  CHECK_INT(131,n,GD_E_BAD_CODE);

  // 133: Dirfile::FrameNum check
  delete d->Reference("data");
  dp = d->FrameNum("data", 33.3, 6);
  CHECK_OK(133);
  CHECK_DOUBLE(133,dp,37.0375);

  // 136: Dirfile::MAdd check
  q[0] = 9.9;
  q[1] = 8.8;
  q[2] = 7.7;
  q[3] = 6.6;
  lent.Dissociate();
  lent.SetName("mnew136");
  lent.SetNFields(2);
  lent.SetInput("in1", 0);
  lent.SetScale(q[0], 0);
  lent.SetOffset(q[1], 0);
  lent.SetInput("in2", 1);
  lent.SetScale(q[2], 1);
  lent.SetOffset(q[3], 1);
  d->MAdd(lent, "data");
  CHECK_OK2(136,1);

  ent = d->Entry("data/mnew136");
  CHECK_OK2(136,2);
  CHECK_INT2(136,1,ent->Type(),LincomEntryType);
  CHECK_INT2(136,2,ent->NFields(),2);
  CHECK_INT2(136,3,ent->FragmentIndex(),0);
  CHECK_STRING2(136,4,ent->Input(0),"in1");
  CHECK_STRING2(136,5,ent->Input(1),"in2");
  CHECK_INT2(136,6,ent->ComplexScalars(),0);
  CHECK_DOUBLE_ARRAY(136,7,2,ent->Scale(i),q[i * 2]);
  CHECK_DOUBLE_ARRAY(136,8,2,ent->Offset(i),q[i * 2 + 1]);
  delete ent;

  // 137: Dirfile::ReferenceFilename check
  str = d->ReferenceFilename();
  CHECK_OK(137);
  sprintf(buf, "dirfile%cdata", GD_DIRSEP);
  CHECK_EOSTRING(137,str, buf);
  
  // 142: Dirfile::BoF check
  n = d->BoF("lincom");
  CHECK_OK(142);
  CHECK_INT(142,n,264);
  
  // 143: Dirfile::Entry / DivideEntry check
  ent = d->Entry("div");
  CHECK_OK(143);
  CHECK_INT2(143,1,ent->Type(),DivideEntryType);
  CHECK_INT2(143,2,ent->FragmentIndex(),0);
  CHECK_STRING2(143,3,ent->Input(0),"mult");
  CHECK_STRING2(143,4,ent->Input(1),"bit");
  delete ent;
  
  // 145: Dirfile::Entry / RecipEntry check
  ent = d->Entry("recip");
  CHECK_OK(145);
  CHECK_INT2(145,1,ent->Type(),RecipEntryType);
  CHECK_INT2(145,2,ent->FragmentIndex(),0);
  CHECK_STRING2(145,3,ent->Input(0),"div");
  CHECK_INT2(145,4,ent->ComplexScalars(),1);
  CHECK_COMPLEX2(145,5,ent->CDividend(),complex<double>(6.5,4.3));
  delete ent;

  // 146: Dirfile::Add / DivideEntry check
  dent.SetName("new14");
  dent.SetFragmentIndex(0);
  dent.SetInput("in1", 0);
  dent.SetInput("in2", 1);
  d->Add(dent);
  CHECK_OK2(146,1);

  ent = d->Entry("new14");
  CHECK_OK2(146,2);
  CHECK_INT2(146,1,ent->Type(),DivideEntryType);
  CHECK_INT2(146,2,ent->FragmentIndex(),0);
  CHECK_STRING2(146,3,ent->Input(0),"in1");
  CHECK_STRING2(146,4,ent->Input(1),"in2");
  delete ent;

  // 147: Dirfile::Add / RecipEntry check
  oent.SetName("new15");
  oent.SetFragmentIndex(0);
  oent.SetInput("in3");
  oent.SetDividend(31.9);
  d->Add(oent);

  ent = d->Entry("new15");
  CHECK_OK2(147,1);
  CHECK_INT2(147,1,ent->Type(),RecipEntryType);
  CHECK_INT2(147,2,ent->FragmentIndex(),0);
  CHECK_STRING2(147,3,ent->Input(0),"in3");
  CHECK_INT2(147,4,ent->ComplexScalars(),0);
  CHECK_DOUBLE2(147,5,ent->Dividend(),31.9);
  delete ent;

  // 148: Dirfile::Add / RecipEntry check
  oent.Dissociate();
  oent.SetName("new16");
  oent.SetFragmentIndex(0);
  oent.SetInput("in2");
  oent.SetDividend(complex<double>(33.3,44.4));
  d->Add(oent);

  ent = d->Entry("new16");
  CHECK_OK2(148,1);
  CHECK_INT2(148,1,ent->Type(),RecipEntryType);
  CHECK_INT2(148,2,ent->FragmentIndex(),0);
  CHECK_STRING2(148,3,ent->Input(0),"in2");
  CHECK_INT2(148,4,ent->ComplexScalars(),1);
  CHECK_COMPLEX2(148,5,ent->CDividend(),complex<double>(33.3,44.4));
  delete ent;

  // 152: DivideEntry check
  dep = reinterpret_cast<DivideEntry*>(d->Entry("new14"));
  CHECK_OK2(152,1);
  dep->SetInput("in4",0);
  CHECK_OK2(152,2);
  dep->SetInput("in5",1);
  CHECK_OK2(152,3);
  delete dep;

  ent = d->Entry("new14");
  CHECK_OK2(152,2);
  CHECK_INT2(152,1,ent->Type(),DivideEntryType);
  CHECK_INT2(152,2,ent->FragmentIndex(),0);
  CHECK_STRING2(152,3,ent->Input(0),"in4");
  CHECK_STRING2(152,4,ent->Input(1),"in5");
  delete ent;

  // 153: RecipEntry check
  oep = reinterpret_cast<RecipEntry*>(d->Entry("new15"));
  CHECK_OK2(153,1);
  oep->SetInput("in1");
  CHECK_OK2(153,2);
  oep->SetDividend(complex<double>(1.01,9.33));
  CHECK_OK2(153,3);
  delete oep;

  ent = d->Entry("new15");
  CHECK_INT2(148,1,ent->Type(),RecipEntryType);
  CHECK_INT2(148,2,ent->FragmentIndex(),0);
  CHECK_STRING2(148,3,ent->Input(0),"in1");
  CHECK_INT2(148,4,ent->ComplexScalars(),1);
  CHECK_COMPLEX2(148,5,ent->CDividend(),complex<double>(1.01,9.33));
  delete ent;

  // 155: Fragment::ReWrite check
  frag->ReWrite();
  CHECK_OK(155);
  delete frag;
  
  // 156: Invalid Dirfile check
  Dirfile *id = new Dirfile();
  CheckError(id,156,1,GD_E_OK);
  id->NFragments();
  CheckError(id,156,2,GD_E_BAD_DIRFILE);
  delete id;

  // 157: Dirfile::Standards check
  n = d->Standards();
  CHECK_OK2(157,1);
  CHECK_INT(157,n,GD_DIRFILE_STANDARDS_VERSION);
  d->Standards(0);
  CHECK_ERROR2(157,2,GD_E_ARGUMENT);

  // 158: gd_get_carray
  n = d->GetCarray("carray", Float64, p);
  CHECK_OK(158);
  CHECK_INT(158,n,0);
  CHECK_DOUBLE_ARRAY(158,1,6,p[i],1.1 * (i + 1));

  // 159: gd_get_carray_slice (INT8)
  n = d->GetCarray("carray", Float64, p, 2, 2);
  CHECK_OK(159);
  CHECK_INT(159,n,0);
  CHECK_DOUBLE_ARRAY(159,1,2,p[i],1.1 * (i + 3));

  // 167: gd_carrays
  carrays = d->Carrays(Float64);
  CHECK_OK(167);
  CHECK_NONNULL(167,carrays);
  CHECK_INT2(167,1,carrays[0].n,6);
  CHECK_DOUBLE_ARRAY(167,2,6,((double*)carrays[0].d)[i],1.1 * (i + 1));
  CHECK_INT2(167,2,carrays[1].n,0);

  // 168: gd_put_carray
  p[0] = 9.6;
  p[1] = 8.5;
  p[2] = 7.4;
  p[3] = 6.3;
  p[4] = 5.2;
  p[5] = 4.1;
  n = d->PutCarray("carray", Float64, p);
  CHECK_OK2(168, 1);

  n = d->GetCarray("carray", Float64, q);
  CHECK_OK2(168,2);
  CHECK_INT(168,n,0);
  CHECK_DOUBLE_ARRAY(168,1,6,q[i],9.6 - i * 1.1);

  // 169: gd_put_carray_slice (INT8)
  p[0] = 2.2;
  p[1] = 3.3;
  n = d->PutCarray("carray", Float64, p, 2, 2);
  CHECK_OK2(168, 1);

  n = d->GetCarray("carray", Float64, q);
  CHECK_OK2(168,2);
  CHECK_INT(168,n,0);
  CHECK_DOUBLE_ARRAY(168,1,6,q[i],(i == 2 || i == 3) ? i * 1.1 : 9.6 - i * 1.1);

  // 177: gd_array_len
  n = (int)d->ArrayLen("carray");
  CHECK_OK(177);
  CHECK_INT(177,n,6);

  // 178: gd_entry (CARRAY)
  ent = d->Entry("carray");
  CHECK_OK(178);
  CHECK_INT2(178,1,ent->Type(),CarrayEntryType);
  CHECK_INT2(178,2,ent->FragmentIndex(),0);
  CHECK_INT2(178,3,ent->ConstType(),Float64);
  CHECK_INT2(178,4,ent->ArrayLen(),6);
  delete ent;

  // 179: gd_add_carray
  aent.SetName("new17");
  aent.SetFragmentIndex(0);
  aent.SetType(Float64);
  aent.SetArrayLen(4);
  d->Add(aent);
  CHECK_OK2(179,1);

  ent = d->Entry("new17");
  CHECK_OK2(179,2);
  CHECK_INT2(179,1,ent->Type(),CarrayEntryType);
  CHECK_INT2(179,2,ent->FragmentIndex(),0);
  CHECK_INT2(179,3,ent->ConstType(),Float64);
  CHECK_INT2(179,4,ent->ArrayLen(),4);
  delete ent;

  // 180: gd_madd_carray
  aent.Dissociate();
  aent.SetName("mnew17");
  aent.SetFragmentIndex(0);
  aent.SetType(Float64);
  aent.SetArrayLen(2);
  d->MAdd(aent, "data");
  CHECK_OK2(180,1);

  ent = d->Entry("data/mnew17");
  CHECK_OK2(180,2);
  CHECK_INT2(180,1,ent->Type(),CarrayEntryType);
  CHECK_INT2(180,2,ent->FragmentIndex(),0);
  CHECK_INT2(180,3,ent->ConstType(),Float64);
  CHECK_INT2(180,4,ent->ArrayLen(),2);
  delete ent;

  // 181: gd_alter_carray
  aep = reinterpret_cast<CarrayEntry*>(d->Entry("new17"));
  CHECK_OK2(181,1);
  aep->SetType(Float32);
  CHECK_OK2(181,2);
  aep->SetArrayLen(12);
  CHECK_OK2(181,3);
  delete aep;

  ent = d->Entry("new17");
  CHECK_OK2(181,2);
  CHECK_INT2(181,1,ent->Type(),CarrayEntryType);
  CHECK_INT2(181,2,ent->FragmentIndex(),0);
  CHECK_INT2(181,3,ent->ConstType(),Float32);
  CHECK_INT2(181,4,ent->ArrayLen(),12);
  delete ent;

  // 183: gd_constants
  p[0] = 86.;
  p[1] = 0.;
  n = d->NFieldsByType(ConstEntryType);
  qp = reinterpret_cast<const double *>(d->Constants());
  CHECK_OK(183);
  CHECK_DOUBLE_ARRAY(183,0,n,qp[i],p[i]);

  // 184: gd_mconstants
  p[0] = 3.3;
  p[1] = 0.;
  n = d->NMFieldsByType("data", ConstEntryType);
  qp = reinterpret_cast<const double *>(d->MConstants("data"));
  CHECK_OK(184);
  CHECK_DOUBLE_ARRAY(184,0,n,qp[i],p[i]);

  // 199: gd_strings
  strings[0] = (char *)"Lorem ipsum";
  strings[1] = (char *)"";
  strings[2] = (char *)"Arthur Dent";
  n = d->NFieldsByType(StringEntryType);
  list = d->Strings();
  CHECK_OK(199);
  CHECK_STRING_ARRAY(199,n,list[i],strings[i]);

  // 200: gd_strings
  strings[0] = (char *)"This is a string constant.";
  n = d->NMFieldsByType("data", StringEntryType);
  list = d->MStrings("data");
  CHECK_OK(200);
  CHECK_STRING_ARRAY(200,n,list[i],strings[i]);

  // 203: gd_seek
  n = d->Seek("data", 35, 0, GD_SEEK_SET);
  CHECK_OK2(203,0);
  m = d->GetData("data", GD_HERE, 0, 1, 0, UInt8, c);
  CHECK_OK2(203,1);
  CHECK_INT2(203,0,n,35 * 8);
  CHECK_INT2(203,1,m,8);
  CHECK_INT_ARRAY(203,8,c[i],17 + i);

  // 204: gd_tell
  n = d->Tell("data");
  CHECK_OK(204);
  CHECK_INT(204,n,288);

  // 205: gd_hide check
  n = d->Hide("data");
  CHECK_OK(205);

  // 206: gd_hidden check
  n = d->Hidden("data");
  CHECK_OK2(206, 1);
  CHECK_INT2(206, 1, n, 1);

  n = d->Hidden("lincom");
  CHECK_OK2(206, 2);
  CHECK_INT2(206, 2, n, 0);

  // 207: gd_unhide check
  n = d->UnHide("data");
  CHECK_OK2(206, 1);
  n = d->Hidden("data");
  CHECK_OK2(206, 2);
  CHECK_INT2(206, 2, n, 0);

  // 208: gd_sync check
  d->Sync("data");
  CHECK_OK(208);

  // 209: gd_flush check
  d->Flush("data");
  CHECK_OK(209);

  // 210: gd_metaflush check
  d->MetaFlush();
  CHECK_OK(210);

  // 211: gd_entry (WINDOW) check
  ent = d->Entry("window");
  CHECK_OK(211);
  CHECK_INT2(211, 1, ent->Type(), WindowEntryType);
  CHECK_INT2(211, 2, ent->FragmentIndex(), 0);
  CHECK_INT2(211, 3, ent->WindOp(), WindOpLt);
  CHECK_STRING2(211, 4, ent->Input(0), "linterp");
  CHECK_STRING2(211, 5, ent->Input(1), "mult");
  CHECK_DOUBLE2(211, 6, ent->Threshold().r, 4.1);
  delete ent;

  // 212: Dirfile::Add / WindowEntry check
  went.SetName("new18");
  went.SetFragmentIndex(0);
  went.SetInput("in1", 0);
  went.SetInput("in2", 1);
  went.SetWindOp(WindOpNe);
  thresh.i = 32;
  went.SetThreshold(thresh);
  d->Add(went);
  CHECK_OK2(212, 1);

  ent = d->Entry("new18");
  CHECK_OK2(212, 2);
  CHECK_INT2(212, 1, ent->Type(), WindowEntryType);
  CHECK_INT2(212, 2, ent->FragmentIndex(), 0);
  CHECK_INT2(212, 3, ent->WindOp(), WindOpNe);
  CHECK_STRING2(212, 4, ent->Input(0), "in1");
  CHECK_STRING2(212, 5, ent->Input(1), "in2");
  CHECK_INT2(212, 6, ent->Threshold().i, 32);
  delete ent;

  // 214: gd_madd_window_i check
  went.Dissociate();
  went.SetName("mnew18");
  went.SetInput("in2", 0);
  went.SetInput("in3", 1);
  went.SetWindOp(WindOpSet);
  thresh.u = 128;
  went.SetThreshold(thresh);
  d->MAdd(went, "data");
  CHECK_OK2(214, 1);

  ent = d->Entry("data/mnew18");
  CHECK_OK2(214, 2);
  CHECK_INT2(214, 1, ent->Type(), WindowEntryType);
  CHECK_INT2(214, 2, ent->FragmentIndex(), 0);
  CHECK_INT2(214, 3, ent->WindOp(), WindOpSet);
  CHECK_STRING2(214, 4, ent->Input(0), "in2");
  CHECK_STRING2(214, 5, ent->Input(1), "in3");
  CHECK_INT2(214, 6, ent->Threshold().u, 128);
  delete ent;

  // 217: gd_alter_window_r check
  wep = reinterpret_cast<WindowEntry*>(d->Entry("new18"));
  wep->SetInput("in3", 0);
  wep->SetInput("in4", 1);
  wep->SetWindOp(WindOpGe);
  thresh.r = 32e3;
  wep->SetThreshold(thresh);
  CHECK_OK2(217, 1);
  delete wep;

  ent = d->Entry("new18");
  CHECK_OK2(217, 2);
  CHECK_INT2(217, 1, ent->Type(), WindowEntryType);
  CHECK_INT2(217, 2, ent->FragmentIndex(), 0);
  CHECK_INT2(217, 3, ent->WindOp(), WindOpGe);
  CHECK_STRING2(217, 4, ent->Input(0), "in3");
  CHECK_STRING2(217, 5, ent->Input(1), "in4");
  CHECK_DOUBLE2(217, 6, ent->Threshold().r, 32e3);
  delete ent;

  // 218: gd_alias_target check
  str = d->AliasTarget("alias");
  CHECK_OK(218);
  CHECK_STRING(218, str, "data");

  // 219: gd_add_alias check
  d->AddAlias("new20", "data", 0);
  CHECK_OK2(219, 1);

  str = d->AliasTarget("new20");
  CHECK_OK2(219, 2);
  CHECK_STRING(219, str, "data");

  // 220: gd_madd_alias check
  d->MAddAlias("data", "mnew20", "data");
  CHECK_OK2(220, 1);

  str = d->AliasTarget("data/mnew20");
  CHECK_OK2(220, 2);
  CHECK_STRING(220, str, "data");

  // 221: gd_naliases check
  n = d->NAliases("data");
  CHECK_OK(221);
  CHECK_INT(221, n, 4);

  // 222: gd_aliases check
  fields[1] = (char*)"data";
  fields[2] = (char*)"alias";
  fields[3] = (char*)"data/mnew20";
  fields[4] = (char*)"new20";
  list = d->Aliases("data");
  CHECK_OK(222);
  CHECK_STRING_ARRAY(222,i,list[i],fields[i]);

  // 223: gd_include_affix check
  d->IncludeAffix("format1", 0, "A", "Z", GD_CREAT | GD_EXCL);
  CHECK_OK(223);

  // 226: gd_fragment_affixes check
  frag = d->Fragment(1);

  CHECK_STRING2(226, 1, frag->Prefix(), "A");
  CHECK_STRING2(226, 2, frag->Suffix(), "Z");

  // 227: gd_alter_affixes check
  frag->SetPrefix("B");
  CHECK_OK2(227, 1);
  frag->SetSuffix("C");
  CHECK_OK2(227, 2);

  CHECK_STRING2(227, 3, frag->Prefix(), "B");
  CHECK_STRING2(227, 3, frag->Suffix(), "C");
  delete frag;

  // 228: gd_entry (MPLEX) check
  ent = d->Entry("mplex");
  CHECK_OK(228);
  CHECK_INT2(228, 1, ent->Type(), MplexEntryType);
  CHECK_INT2(228, 2, ent->FragmentIndex(), 0);
  CHECK_INT2(228, 3, ent->CountVal(), 1);
  CHECK_STRING2(228, 4, ent->Input(0), "data");
  CHECK_STRING2(228, 5, ent->Input(1), "sbit");
  CHECK_INT2(228, 6, ent->Period(), 10);
  delete ent;

  // 229: Dirfile::Add / MplexEntry check
  xent.SetName("new21");
  xent.SetFragmentIndex(0);
  xent.SetInput("in1", 0);
  xent.SetInput("in2", 1);
  xent.SetCountVal(5);
  xent.SetPeriod(6);
  d->Add(xent);
  CHECK_OK2(229, 1);

  ent = d->Entry("new21");
  CHECK_OK2(229, 2);
  CHECK_INT2(229, 1, ent->Type(), MplexEntryType);
  CHECK_INT2(229, 2, ent->FragmentIndex(), 0);
  CHECK_INT2(229, 3, ent->CountVal(), 5);
  CHECK_STRING2(229, 4, ent->Input(0), "in1");
  CHECK_STRING2(229, 5, ent->Input(1), "in2");
  CHECK_INT2(229, 6, ent->Period(), 6);
  delete ent;

  // 230: gd_madd_mplex check
  xent.Dissociate();
  xent.SetName("mnew21");
  xent.SetInput("in2", 0);
  xent.SetInput("in3", 1);
  xent.SetCountVal(0);
  xent.SetPeriod(12);
  d->MAdd(xent, "data");
  CHECK_OK2(230, 1);

  ent = d->Entry("data/mnew21");
  CHECK_OK2(230, 2);
  CHECK_INT2(230, 1, ent->Type(), MplexEntryType);
  CHECK_INT2(230, 2, ent->FragmentIndex(), 0);
  CHECK_INT2(230, 3, ent->CountVal(), 0);
  CHECK_STRING2(230, 4, ent->Input(0), "in2");
  CHECK_STRING2(230, 5, ent->Input(1), "in3");
  CHECK_INT2(230, 6, ent->Period(), 12);
  delete ent;

  // 231: gd_alter_mplex check
  xep = reinterpret_cast<MplexEntry*>(d->Entry("new21"));
  xep->SetInput("in3", 0);
  xep->SetInput("in4", 1);
  xep->SetCountVal(3);
  xep->SetPeriod(7);
  CHECK_OK2(231, 1);
  delete xep;

  ent = d->Entry("new21");
  CHECK_OK2(231, 2);
  CHECK_INT2(231, 1, ent->Type(), MplexEntryType);
  CHECK_INT2(231, 2, ent->FragmentIndex(), 0);
  CHECK_INT2(231, 3, ent->CountVal(), 3);
  CHECK_STRING2(231, 4, ent->Input(0), "in3");
  CHECK_STRING2(231, 5, ent->Input(1), "in4");
  CHECK_INT2(231, 6, ent->Period(), 7);
  delete ent;

  // 232: gd_tokenise
  tok = d->StrTok("\"test1 test2\" test3\\ test4");
  CHECK_OK2(232, 1);
  CHECK_STRING2(232, 2, tok, "test1 test2");
  free(tok);

  tok = d->StrTok();
  CHECK_OK2(232, 3);
  CHECK_STRING2(232, 4, tok, "test3 test4");
  free(tok);

  // 233: gd_raw_close check
  d->RawClose("data");
  CHECK_OK(233);

  // 234: gd_desync check
  n = d->DeSync();
  CHECK_OK(234);
  CHECK_INT(234, n, 0);

  // 235: gd_flags check
  n = d->Flags(GD_PRETTY_PRINT, 0);
  CHECK_OK(235);
  CHECK_INT(235, n, GD_PRETTY_PRINT);

  // 236: gd_verbose_prefix
  d->VerbosePrefix("big_test: ");
  CHECK_OK(236);

  // 237: gd_nentries
  n = d->NEntries("data", GD_SCALAR_ENTRIES,
      GD_ENTRIES_HIDDEN | GD_ENTRIES_NOALIAS);
  CHECK_OK2(237, 1);
  CHECK_INT2(237, 1, n, 5);
  n = d->NEntries(NULL, GD_VECTOR_ENTRIES,
      GD_ENTRIES_HIDDEN | GD_ENTRIES_NOALIAS);
  CHECK_OK2(237, 2);
  CHECK_INT2(237, 2, n, 27);

  // 239: gd_entry_list
  fields[ 0] = (char*)"bit";
  fields[ 1] = (char*)"div";
  fields[ 2] = (char*)"data";
  fields[ 3] = (char*)"mult";
  fields[ 4] = (char*)"new1";
  fields[ 5] = (char*)"new2";
  fields[ 6] = (char*)"new3";
  fields[ 7] = (char*)"new4";
  fields[ 8] = (char*)"new5";
  fields[ 9] = (char*)"new6";
  fields[10] = (char*)"new7";
  fields[11] = (char*)"new8";
  fields[12] = (char*)"sbit";
  fields[13] = (char*)"INDEX";
  fields[14] = (char*)"indir";
  fields[15] = (char*)"mplex";
  fields[16] = (char*)"new14";
  fields[17] = (char*)"new15";
  fields[18] = (char*)"new16";
  fields[19] = (char*)"new18";
  fields[20] = (char*)"new21";
  fields[21] = (char*)"phase";
  fields[22] = (char*)"recip";
  fields[23] = (char*)"lincom";
  fields[24] = (char*)"window";
  fields[25] = (char*)"linterp";
  fields[26] = (char*)"polynom";
  list = d->EntryList(NULL, GD_VECTOR_ENTRIES,
      GD_ENTRIES_HIDDEN | GD_ENTRIES_NOALIAS);
  CHECK_OK(239);
  CHECK_STRING_ARRAY(239,n,list[i],fields[i]);

  // 240: gd_mplex_lookback
  d->MplexLookback(GD_LOOKBACK_ALL);
  CHECK_OK(240);

  // 241: gd_linterp_tablename check
  tok = d->LinterpTableName("linterp");
  CHECK_OK(241);
  sprintf(buf, "dirfile%clut", GD_DIRSEP);
  CHECK_EOSTRING(241,tok,buf);
  free(tok);

  // 242: gd_carrays
  carrays = d->MCarrays("data", Float64);
  CHECK_OK(242);
  CHECK_NONNULL(242,carrays);
  CHECK_INT2(242,1,carrays[0].n,5);
  CHECK_DOUBLE_ARRAY(242,2,5,((double*)carrays[0].d)[i],(1.9 + i * 0.9));
  CHECK_INT2(242,3,carrays[1].n,2);
  CHECK_DOUBLE_ARRAY(242,4,2,((double*)carrays[1].d)[i],0);
  CHECK_INT2(242,5,carrays[2].n,0);

  // 271: gd_encoding_support
  n = EncodingSupport(GetData::SieEncoding);
  CHECK_INT(271, n, GD_RDWR);

  // 272: NULL return from gd_reference
  id = new Dirfile("dirfile/empty", GD_RDWR | GD_CREAT | GD_EXCL);
  CHECK_OK2(272, 1);

  rep = id->Reference();
  CHECK_OK2(272, 2);
  CHECK_NULL(272, rep);

  delete id;

  // 277: gd_entry (SARRAY)
  ent = d->Entry("sarray");
  CHECK_OK(277);
  CHECK_INT2(277,1,ent->Type(),SarrayEntryType);
  CHECK_INT2(277,2,ent->FragmentIndex(),0);
  CHECK_INT2(277,3,ent->ArrayLen(),7);
  delete ent;

  // 278: gd_get_sarray
  fields[0] = (char*)"one";
  fields[1] = (char*)"two";
  fields[2] = (char*)"three";
  fields[3] = (char*)"four";
  fields[4] = (char*)"five";
  fields[5] = (char*)"six";
  fields[6] = (char*)"seven";
  n = d->GetSarray("sarray", list);
  CHECK_OK(278);
  CHECK_INT(278,n,0);
  CHECK_STRING_ARRAY(278,7,list[i],fields[i]);

  // 279: gd_get_sarray_slice
  n = d->GetSarray("sarray", list, 4, 3);
  CHECK_OK(279);
  CHECK_INT(279,n,0);
  CHECK_STRING_ARRAY(279,3,list[i],fields[i+4]);

  // 280: gd_sarrays
  sarrays = d->Sarrays();
  CHECK_OK(280);
  CHECK_NONNULL(280,sarrays);
  for (i = 0; sarrays[i] != NULL; ++i) {
    for (j = 0; sarrays[i][j] != NULL; ++j)
      CHECK_STRINGi(280, i * 1000 + j, sarrays[i][j], fields[j]);
    CHECK_INT2(280, i + 1, j, 7);
  }
  CHECK_INT2(280, 0, i, 1);

  // 281: gd_put_sarray
  fields[0] = (char*)"eka";
  fields[1] = (char*)"dvi";
  fields[2] = (char*)"tri";
  fields[3] = (char*)"catur";
  fields[4] = (char*)"panca";
  fields[5] = (char*)"sas";
  fields[6] = (char*)"sapta";
  n = d->PutSarray("sarray", (const char**)fields);
  CHECK_OK2(281, 1);

  n = d->GetSarray("sarray", list);
  CHECK_OK2(281,2);
  CHECK_INT(281,n,0);
  CHECK_STRING_ARRAY(281,7,list[i],fields[i]);

  // 282: gd_put_sarray_slice
  fields[4] = (char*)"asta";
  fields[5] = (char*)"nava";
  n = d->PutSarray("sarray", (const char**)fields + 4, 4, 2);
  CHECK_OK2(282, 1);

  n = d->GetSarray("sarray", list);
  CHECK_OK2(282,2);
  CHECK_INT(282,n,0);
  CHECK_STRING_ARRAY(282,7,list[i],fields[i]);

  // 283: gd_add_sarray
  saent.SetName("new283");
  saent.SetFragmentIndex(0);
  saent.SetArrayLen(4);
  d->Add(saent);
  CHECK_OK2(283,1);

  ent = d->Entry("new283");
  CHECK_OK2(283,2);
  CHECK_INT2(283,1,ent->Type(),SarrayEntryType);
  CHECK_INT2(283,2,ent->FragmentIndex(),0);
  CHECK_INT2(283,4,ent->ArrayLen(),4);
  delete ent;

  // 285: gd_madd_sarray
  saent.Dissociate();
  saent.SetName("mnew285");
  saent.SetFragmentIndex(0);
  saent.SetArrayLen(2);
  d->MAdd(saent, "data");
  CHECK_OK2(285,1);

  ent = d->Entry("data/mnew285");
  CHECK_OK2(285,2);
  CHECK_INT2(285,1,ent->Type(),SarrayEntryType);
  CHECK_INT2(285,2,ent->FragmentIndex(),0);
  CHECK_INT2(285,4,ent->ArrayLen(),2);
  delete ent;

  // 286: gd_alter_sarray
  saep = reinterpret_cast<SarrayEntry*>(d->Entry("new283"));
  CHECK_OK2(286,1);
  saep->SetArrayLen(12);
  CHECK_OK2(286,2);
  delete saep;

  ent = d->Entry("new283");
  CHECK_OK2(286,3);
  CHECK_INT2(286,1,ent->Type(),SarrayEntryType);
  CHECK_INT2(286,2,ent->FragmentIndex(),0);
  CHECK_INT2(286,4,ent->ArrayLen(),12);
  delete ent;

  // 287: gd_msarrays
  fields[0] = (char*)"eight";
  fields[1] = (char*)"nine";
  fields[2] = (char*)"ten";
  fields[3] = (char*)"eleven";
  fields[4] = (char*)"twelve";
  sarrays = d->MSarrays("data");
  CHECK_OK(287);
  CHECK_NONNULL2(287,0,sarrays);
  CHECK_NONNULL2(287,1,sarrays[0]);
  for (j = 0; sarrays[0][j] != NULL; ++j)
    CHECK_STRINGi(287, j, sarrays[0][j], fields[j]);
  CHECK_INT2(287, 1, j, 5);

  fields[0] = (char*)"";
  fields[1] = (char*)"";
  CHECK_NONNULL2(287,2,sarrays[1]);
  for (j = 0; sarrays[1][j] != NULL; ++j)
    CHECK_STRINGi(287, 1000 + j, sarrays[1][j], fields[j]);
  CHECK_INT2(287, 3, j, 2);
  CHECK_NULL2(287,4,sarrays[2]);

  // 288: Dirfile::Entry / IndirEntry check
  ent = d->Entry("indir");
  CHECK_OK(288);
  CHECK_INT2(288,1,ent->Type(),IndirEntryType);
  CHECK_INT2(288,2,ent->FragmentIndex(),0);
  CHECK_STRING2(288,3,ent->Input(0),"data");
  CHECK_STRING2(288,4,ent->Input(1),"carray");
  delete ent;

  // 289: Dirfile::Add / IndirEntry check
  ient.SetName("new289");
  ient.SetFragmentIndex(0);
  ient.SetInput("in1", 0);
  ient.SetInput("in2", 1);
  d->Add(ient);
  CHECK_OK2(289,1);

  ent = d->Entry("new289");
  CHECK_OK2(289,2);
  CHECK_INT2(289,1,ent->Type(),IndirEntryType);
  CHECK_INT2(289,2,ent->FragmentIndex(),0);
  CHECK_STRING2(289,3,ent->Input(0),"in1");
  CHECK_STRING2(289,4,ent->Input(1),"in2");

  // 291: IndirEntry check
  iep = reinterpret_cast<IndirEntry*>(d->Entry("new289"));
  CHECK_OK2(291,1);
  iep->SetInput("in4",0);
  CHECK_OK2(291,2);
  iep->SetInput("in5",1);
  CHECK_OK2(291,3);
  delete iep;

  ent = d->Entry("new289");
  CHECK_OK2(291,2);
  CHECK_INT2(291,1,ent->Type(),IndirEntryType);
  CHECK_INT2(291,2,ent->FragmentIndex(),0);
  CHECK_STRING2(291,3,ent->Input(0),"in4");
  CHECK_STRING2(291,4,ent->Input(1),"in5");

  // 292: Dirfile::Entry / SindirEntry check
  ent = d->Entry("sindir");
  CHECK_OK(292);
  CHECK_INT2(292,1,ent->Type(),SindirEntryType);
  CHECK_INT2(292,2,ent->FragmentIndex(),0);
  CHECK_STRING2(292,3,ent->Input(0),"data");
  CHECK_STRING2(292,4,ent->Input(1),"sarray");
  delete ent;

  // 293: Dirfile::Add / SindirEntry check
  sient.SetName("new293");
  sient.SetFragmentIndex(0);
  sient.SetInput("in1", 0);
  sient.SetInput("in2", 1);
  d->Add(sient);
  CHECK_OK2(293,1);

  ent = d->Entry("new293");
  CHECK_OK2(293,2);
  CHECK_INT2(293,1,ent->Type(),SindirEntryType);
  CHECK_INT2(293,2,ent->FragmentIndex(),0);
  CHECK_STRING2(293,3,ent->Input(0),"in1");
  CHECK_STRING2(293,4,ent->Input(1),"in2");

  // 295: SindirEntry check
  siep = reinterpret_cast<SindirEntry*>(d->Entry("new293"));
  CHECK_OK2(295,1);
  siep->SetInput("in4",0);
  CHECK_OK2(295,2);
  siep->SetInput("in5",1);
  CHECK_OK2(295,3);
  delete siep;

  ent = d->Entry("new293");
  CHECK_OK2(295,2);
  CHECK_INT2(295,1,ent->Type(),SindirEntryType);
  CHECK_INT2(295,2,ent->FragmentIndex(),0);
  CHECK_STRING2(295,3,ent->Input(0),"in4");
  CHECK_STRING2(295,4,ent->Input(1),"in5");
  delete ent;

  // 296: gd_getstrdata
  n = d->GetData("sindir", 0, 0, 1, 0, list);
  CHECK_OK(296);
  CHECK_INT(296,n,8);
  CHECK_STRING_ARRAY(296,8,list[i],"eka");

  // 302: gd_include_ns
  d->IncludeNS("format2", 0, "ns", GD_CREAT | GD_EXCL);
  CHECK_OK(302);

  // 303: Get Namespace
  frag = d->Fragment(2);
  CHECK_STRING(303, frag->Namespace(), "ns");

  // 304: Set Namespace
  n = frag->SetNamespace("ns2");
  CHECK_OK(304);
  CHECK_INT(304, n, 0);
  CHECK_STRING(304, frag->Namespace(), "ns2");

  // 305: gd_match_entries
#ifndef GD_NO_REGEX
  n = d->MatchEntries("^lin", 0, 0, 0, &list);
  CHECK_OK(305);
  CHECK_INT(305, n, 2);
  CHECK_STRINGi(287, 0, list[0], "lincom");
  CHECK_STRINGi(287, 1, list[1],  "linterp");
#endif







  // ===================================================================
  d->Discard();
  delete d;
  unlink(eformat);
  rmdir(empty);
  unlink(data);
  unlink(new1);
  unlink(format);
  unlink(format1);
  unlink(format2);
  unlink(form2);
  rmdir(filedir);
}

int main(void)
{
  run_tests();

  if (ne) {
    cerr << "ne = " << ne << endl;
    return 1;
  }

  return 0;
}
