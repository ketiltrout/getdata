#include "getdata/dirfile.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <iostream>
#include <math.h>

using namespace std;
using namespace GetData;

#define CHECK_ERROR(t,g) \
  e = d->Error(); if (e != (g)) { ne++; cerr << "e[" << t << "] = " << e << endl; }
#define CHECK_ERROR2(t,n,g) \
  e = d->Error(); if (e != (g)) { \
    ne++; cerr << "e[" << t << ", " << n << "] = " << e << endl; }
#define CHECK_OK(t) CHECK_ERROR(t,GD_E_OK)
#define CHECK_OK2(t,n) CHECK_ERROR2(t,n,GD_E_OK)

#define CHECK_INT(t,v,g) \
  if ((v) != (g)) { ne++; cerr << "n[" << t << "] = " << (v) << endl; }
#define CHECK_INT2(t,n,v,g) \
  if ((v) != (g)) { \
    ne++; cerr << "n[" << t << ", " << n << "] = " << (v) << endl; }
#define CHECK_INT_ARRAY(t,m,v,g) \
  for (i = 0; i < m; ++i) if ((v) != (g)) { \
    ne++; cerr << "n(" << i << ")[" << t << "] = " << (int)v << endl; }

#define CHECK_DOUBLE(t,v,g) \
  if (fabs((v) - (g)) > 1e-10) { \
    ne++; cerr << "d[" << t << "] = " << (v) << endl; }
#define CHECK_DOUBLE2(t,m,v,g) \
  if (fabs((v) - (g)) > 1e-10) { \
    ne++; cerr << "d[" << t << ", " << m << "] = " << (v) << endl; }
#define CHECK_DOUBLE_ARRAY(t,m,n,v,g) \
  for (i = 0; i < n; ++i) if (fabs((v) - (g)) > 1e-10) { \
    ne++; cerr << "d(" << i << ")[" << t << ", " << m << "] = " << (v) << endl; }

#define CHECK_STRING(t,v,g) \
  if (strcmp((v), (g))) { ne++; cerr << "s[" << t << "] = " << (v) << endl; }
#define CHECK_STRING2(t,m,v,g) \
  if (strcmp((v), (g))) { \
    ne++; cerr << "s[" << t << ", " << m << "] = " << (v) << endl; }
#define CHECK_STRING_ARRAY(t,m,v,g) \
  for (i = 0; i < m; ++i) if (strcmp((v), (g))) { \
    ne++; cerr << "s(" << i << ")[" << t << "] = " << (v) << endl; }

#define CHECK_COMPLEX_ARRAY(t,m,v,g) \
  for (i = 0; i < m; ++i) if (abs((v) - (g)) > 1e-10) { \
    ne++; cerr << "c(" << i << ")[" << t << "] = " << v.real() \
    << ";" << v.imag() << endl; }

int main(void)
{
  const char* filedir = __TEST__ "dirfile";
  const char* format = __TEST__ "dirfile/format";
  const char* form2 = __TEST__ "dirfile/form2";
  const char* new1 = __TEST__ "dirfile/new1";
  const char* data = __TEST__ "dirfile/data";
  const char* format_data =
    "/ENDIAN little\n"
    "data RAW INT8 8\n"
    "lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp const const\n"
    "/META data mstr STRING \"This is a string constant.\"\n"
    "/META data mconst CONST COMPLEX128 3.3;4.4\n"
    "/META data mlut LINTERP DATA ./lut\n"
    "const CONST FLOAT64 5.5\n"
    "linterp LINTERP data /look/up/file\n"
    "polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const\n"
    "bit BIT data 3 4\n"
    "sbit SBIT data 5 6\n"
    "mult MULTIPLY data sbit\n"
    "phase PHASE data 11\n"
    "string STRING \"Zaphod Beeblebrox\"\n";
  const char* form2_data = "const2 CONST INT8 -19\n";
  const int nfields = 11;
  unsigned char c[8];
  unsigned char data_data[80];
  signed char sc;
  int n, i, e, ne = 0;
  float fl;
  double dp, q[6];
  complex<double> cq[6];
  const char **list;
  const char* str;
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
  SBitEntry sent, *sep;
  ConstEntry cent, *cep;
  StringEntry gent;
  Fragment *frag;

  char* fields[nfields + 9] = {(char*)"INDEX", (char*)"bit", (char*)"const",
    (char*)"data", (char*)"lincom", (char*)"linterp", (char*)"mult",
    (char*)"phase", (char*)"polynom", (char*)"sbit", (char*)"string", NULL,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

  // Write the test dirfile
  mkdir(filedir, 0777);

  for (n = 0; n < 80; ++n)
    data_data[n] = (unsigned char)n + 1;

  n = open(format, O_CREAT | O_TRUNC | O_WRONLY, 0666);
  write(n, format_data, strlen(format_data));
  close(n);

  n = open(form2, O_CREAT | O_TRUNC | O_WRONLY, 0666);
  write(n, form2_data, strlen(form2_data));
  close(n);

  n = open(data, O_CREAT | O_TRUNC | O_WRONLY, 0666);
  write(n, data_data, 80);
  close(n);

  // 0: Dirfile::Error check
  d = new Dirfile("x");
  CHECK_ERROR(0, GD_E_OPEN);
  delete d;

  // 1: Dirfile::Dirfile check
  d = new Dirfile(filedir, GD_RDWR);
  CHECK_OK(1);

  // 2: Dirfile::GetData check
  n = d->GetData("data", 5, 0, 1, 0, UInt8, c);
  CHECK_OK(2);
  CHECK_INT(2,n,8);
  CHECK_INT_ARRAY(2,8,c[i],41 + i);

  // 3: Dirfile::GetConstant check
  n = d->GetConstant("const", Float64, &dp);
  CHECK_OK(3);
  CHECK_INT(3,n,0);
  CHECK_DOUBLE(3,dp,5.5);

  // 6: Dirfile::NFields check
  n = d->NFields();
  CHECK_OK(6);
  CHECK_INT(6,n,nfields);

  // 8: Dirfile::FieldList check
  list = d->FieldList();
  CHECK_OK(8);
  CHECK_STRING_ARRAY(8,n,list[i],fields[i]);

  // 9: Dirfile::NFields check
  n = d->NMFields("data");
  CHECK_OK(9);
  CHECK_INT(9,n,3);

  // 10: Dirfile::MFieldList check
  fields[0] = (char*)"mstr";
  fields[1] = (char*)"mconst";
  fields[2] = (char*)"mlut";
  list = d->MFieldList("data");
  CHECK_OK(10);
  CHECK_STRING_ARRAY(10,n,list[i],fields[i]);

  // 11: Dirfile::NFrames check
  n = d->NFrames();
  CHECK_OK(11);
  CHECK_INT(11,n,10);

  // 12: Dirfile::SamplesPerFrame check
  n = d->SamplesPerFrame("data");
  CHECK_OK(12);
  CHECK_INT(12,n,8);

  // 13: Dirfile::PutData check
  c[0] = 13;
  c[1] = 14;
  c[2] = 15;
  c[3] = 16;
  n = d->PutData("data", 5, 1, 0, 4, UInt8, c);
  CHECK_OK2(13,1);
  CHECK_INT2(13,1,n,4);

  n = d->GetData("data", 5, 0, 1, 0, UInt8, c);
  CHECK_OK2(13,2);
  CHECK_INT2(13,2,n,8);
  CHECK_INT_ARRAY(13,8,c[i],(i == 0 || i > 4) ? 41 + i : 12 + i);

  // 14: Dirfile::ErrorString check
  d->GetData("x", 5, 0, 1, 0, Null, NULL);
  str = d->ErrorString();
  CHECK_STRING(14,str,"Field not found: x");

  // 16: Dirfile::Entry / RawEntry check
  ent = d->Entry("data");
  CHECK_OK(16);
  CHECK_INT2(16,1,ent->Type(),RawEntryType);
  CHECK_INT2(16,2,ent->FragmentIndex(),0);
  CHECK_INT2(16,3,ent->SamplesPerFrame(),8);
  CHECK_INT2(16,4,ent->RawType(),Int8);

  // 18: Dirfile::Entry / LincomEntry check
  cq[0] = 1.1;
  cq[1] = 2.2;
  cq[2] = 2.2;
  cq[3] = complex<double>(3.3, 4.4);
  cq[4] = 5.5;
  cq[5] = 5.5;
  ent = d->Entry("lincom");
  CHECK_OK(18);
  CHECK_INT2(18,1,ent->Type(),LincomEntryType);
  CHECK_INT2(18,2,ent->NFields(),3);
  CHECK_INT2(18,3,ent->FragmentIndex(),0);
  CHECK_STRING2(18,4,ent->Input(0),"data");
  CHECK_STRING2(18,5,ent->Input(1),"INDEX");
  CHECK_STRING2(18,6,ent->Input(2),"linterp");
  CHECK_INT2(18,7,ent->ComplexScalars(),1);
  CHECK_COMPLEX_ARRAY(18,3,ent->CScale(i),cq[i * 2]);
  CHECK_COMPLEX_ARRAY(18,3,ent->COffset(i),cq[i * 2 + 1]);

  // 20: Dirfile::Entry / PolynomEntry check
  ent = d->Entry("polynom");
  CHECK_OK(20);
  CHECK_INT2(20,1,ent->Type(),PolynomEntryType);
  CHECK_INT2(20,2,ent->PolyOrd(),5);
  CHECK_INT2(20,3,ent->FragmentIndex(),0);
  CHECK_STRING2(20,4,ent->Input(),"data");
  CHECK_INT2(20,7,ent->ComplexScalars(),1);
  CHECK_COMPLEX_ARRAY(20,6,ent->CCoefficient(i),cq[i]);

  // 21: Dirfile::Entry / LinterpEntry check
  ent = d->Entry("linterp");
  CHECK_OK(21);
  CHECK_INT2(21,1,ent->Type(),LinterpEntryType);
  CHECK_INT2(21,2,ent->FragmentIndex(),0);
  CHECK_STRING2(21,3,ent->Input(),"data");
  CHECK_STRING2(21,4,ent->Table(),"/look/up/file");

  // 22: Dirfile::Entry / BitEntry check
  ent = d->Entry("bit");
  CHECK_OK(22);
  CHECK_INT2(22,1,ent->Type(),BitEntryType);
  CHECK_INT2(22,2,ent->FragmentIndex(),0);
  CHECK_STRING2(22,3,ent->Input(),"data");
  CHECK_INT2(22,4,ent->NumBits(),4);
  CHECK_INT2(22,5,ent->FirstBit(),3);

  // 23: Dirfile::Entry / SBitEntry check
  ent = d->Entry("sbit");
  CHECK_OK(23);
  CHECK_INT2(23,1,ent->Type(),SBitEntryType);
  CHECK_INT2(23,2,ent->FragmentIndex(),0);
  CHECK_STRING2(23,3,ent->Input(),"data");
  CHECK_INT2(23,4,ent->NumBits(),6);
  CHECK_INT2(23,5,ent->FirstBit(),5);

  // 24: Dirfile::Entry / MultiplyEntry check
  ent = d->Entry("mult");
  CHECK_OK(24);
  CHECK_INT2(24,1,ent->Type(),MultiplyEntryType);
  CHECK_INT2(24,2,ent->FragmentIndex(),0);
  CHECK_STRING2(24,3,ent->Input(0),"data");
  CHECK_STRING2(24,4,ent->Input(1),"sbit");

  // 25: Dirfile::Entry / PhaseEntry check
  ent = d->Entry("phase");
  CHECK_OK(25);
  CHECK_INT2(25,1,ent->Type(),PhaseEntryType);
  CHECK_INT2(25,2,ent->FragmentIndex(),0);
  CHECK_STRING2(25,3,ent->Input(),"data");
  CHECK_INT2(25,4,ent->Shift(),11);

  // 26: Dirfile::Entry / ConstEntry check
  ent = d->Entry("const");
  CHECK_OK(26);
  CHECK_INT2(26,1,ent->Type(),ConstEntryType);
  CHECK_INT2(26,2,ent->FragmentIndex(),0);
  CHECK_INT2(26,3,ent->ConstType(),Float64);

  // 134: Dirfile::Entry / StringEntry check
  ent = d->Entry("string");
  CHECK_OK(134);
  CHECK_INT2(134,1,ent->Type(),StringEntryType);
  CHECK_INT2(134,2,ent->FragmentIndex(),0);

  // 27: Dirfile::FragmentIndex check
  n = d->FragmentIndex("data");
  CHECK_OK(27);
  CHECK_INT(27,n,0);

  // 28: Dirfile::Add / RawEntry check
  rent.SetName("new1");
  rent.SetFragmentIndex(0);
  rent.SetSamplesPerFrame(3);
  rent.SetType(Float64);
  d->Add(rent);
  CHECK_OK2(28,1);

  ent = d->Entry("new1");
  CHECK_OK2(28,2);
  CHECK_INT2(28,1,ent->Type(),RawEntryType);
  CHECK_INT2(28,2,ent->FragmentIndex(),0);
  CHECK_INT2(28,3,ent->SamplesPerFrame(),3);
  CHECK_INT2(28,4,ent->RawType(),Float64);

  // 29: Dirfile::Add / LincomEntry check
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
  CHECK_OK2(29,1);

  ent = d->Entry("new2");
  CHECK_OK2(29,2);
  CHECK_INT2(29,1,ent->Type(),LincomEntryType);
  CHECK_INT2(29,2,ent->NFields(),2);
  CHECK_INT2(29,3,ent->FragmentIndex(),0);
  CHECK_STRING2(29,4,ent->Input(0),"in1");
  CHECK_STRING2(29,5,ent->Input(1),"in2");
  CHECK_INT2(29,6,ent->ComplexScalars(),0);
  CHECK_DOUBLE_ARRAY(29,7,2,ent->Scale(i),q[i * 2]);
  CHECK_DOUBLE_ARRAY(29,8,2,ent->Offset(i),q[i * 2 + 1]);

  // 30: Dirfile::Add / LincomEntry check
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
  CHECK_OK2(30,1);

  ent = d->Entry("new3");
  CHECK_OK2(30,2);
  CHECK_INT2(30,1,ent->Type(),LincomEntryType);
  CHECK_INT2(30,2,ent->NFields(),2);
  CHECK_INT2(30,3,ent->FragmentIndex(),0);
  CHECK_STRING2(30,4,ent->Input(0),"in1");
  CHECK_STRING2(30,5,ent->Input(1),"in2");
  CHECK_INT2(30,6,ent->ComplexScalars(),1);
  CHECK_COMPLEX_ARRAY(30,2,ent->CScale(i),cq[i * 2]);
  CHECK_COMPLEX_ARRAY(30,2,ent->COffset(i),cq[i * 2 + 1]);

  // 31: Dirfile::Add / PolynomEntry check
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
  CHECK_OK2(31,1);

  ent = d->Entry("new4");
  CHECK_OK2(31,2);
  CHECK_INT2(31,1,ent->Type(),PolynomEntryType);
  CHECK_INT2(31,2,ent->PolyOrd(),3);
  CHECK_INT2(31,3,ent->FragmentIndex(),0);
  CHECK_STRING2(31,4,ent->Input(),"in1");
  CHECK_INT2(31,5,ent->ComplexScalars(),0);
  CHECK_DOUBLE_ARRAY(31,6,4,ent->Coefficient(i),q[i]);

  // 32: Dirfile::Add / PolynomEntry check
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
  CHECK_OK2(32,1);

  ent = d->Entry("new5");
  CHECK_OK2(32,2);
  CHECK_INT2(32,1,ent->Type(),PolynomEntryType);
  CHECK_INT2(32,2,ent->PolyOrd(),3);
  CHECK_INT2(32,3,ent->FragmentIndex(),0);
  CHECK_STRING2(32,4,ent->Input(),"in2");
  CHECK_INT2(32,7,ent->ComplexScalars(),1);
  CHECK_COMPLEX_ARRAY(32,4,ent->CCoefficient(i),cq[i]);

  // 33: Dirfile::Add / LinterpEntry check
  nent.SetName("new6");
  nent.SetFragmentIndex(0);
  nent.SetInput("in");
  nent.SetTable("./some/table");
  d->Add(nent);
  CHECK_OK2(33,1);

  ent = d->Entry("new6");
  CHECK_OK2(33,2);
  CHECK_INT2(33,1,ent->Type(),LinterpEntryType);
  CHECK_INT2(33,2,ent->FragmentIndex(),0);
  CHECK_STRING2(33,3,ent->Input(),"in");
  CHECK_STRING2(33,4,ent->Table(),"./some/table");

  // 34: Dirfile::Add / BitEntry check
  bent.SetName("new7");
  bent.SetFragmentIndex(0);
  bent.SetInput("in1");
  bent.SetFirstBit(13);
  bent.SetNumBits(12);
  d->Add(bent);
  CHECK_OK2(34,1);

  ent = d->Entry("new7");
  CHECK_OK(34);
  CHECK_INT2(34,1,ent->Type(),BitEntryType);
  CHECK_INT2(34,2,ent->FragmentIndex(),0);
  CHECK_STRING2(34,3,ent->Input(),"in1");
  CHECK_INT2(34,4,ent->NumBits(),12);
  CHECK_INT2(34,5,ent->FirstBit(),13);

  // 35: Dirfile::Add / SBitEntry check
  sent.SetName("new8");
  sent.SetFragmentIndex(0);
  sent.SetInput("in2");
  sent.SetFirstBit(14);
  sent.SetNumBits(15);
  d->Add(sent);
  CHECK_OK2(35,1);

  ent = d->Entry("new8");
  CHECK_OK(35);
  CHECK_INT2(35,1,ent->Type(),SBitEntryType);
  CHECK_INT2(35,2,ent->FragmentIndex(),0);
  CHECK_STRING2(35,3,ent->Input(),"in2");
  CHECK_INT2(35,4,ent->NumBits(),15);
  CHECK_INT2(35,5,ent->FirstBit(),14);

  // 36: Dirfile::Add / MultiplyEntry check
  ment.SetName("new9");
  ment.SetFragmentIndex(0);
  ment.SetInput("in1", 0);
  ment.SetInput("in2", 1);
  d->Add(ment);
  CHECK_OK2(36,1);

  ent = d->Entry("new9");
  CHECK_OK2(36,2);
  CHECK_INT2(36,1,ent->Type(),MultiplyEntryType);
  CHECK_INT2(36,2,ent->FragmentIndex(),0);
  CHECK_STRING2(36,3,ent->Input(0),"in1");
  CHECK_STRING2(36,4,ent->Input(1),"in2");

  // 37: Dirfile::Add / PhaseEntry check
  pent.SetName("new10");
  pent.SetFragmentIndex(0);
  pent.SetInput("in1");
  pent.SetShift(22);
  d->Add(pent);
  CHECK_OK2(37,1);

  ent = d->Entry("new10");
  CHECK_OK(37);
  CHECK_INT2(37,1,ent->Type(),PhaseEntryType);
  CHECK_INT2(37,2,ent->FragmentIndex(),0);
  CHECK_STRING2(37,3,ent->Input(),"in1");
  CHECK_INT2(37,4,ent->Shift(),22);

  // 38: Dirfile::Add / ConstEntry check
  cent.SetName("new11");
  cent.SetFragmentIndex(0);
  cent.SetType(Float64);
  d->Add(cent);
  CHECK_OK2(38,1);

  ent = d->Entry("new11");
  CHECK_OK2(38,2);
  CHECK_INT2(38,1,ent->Type(),ConstEntryType);
  CHECK_INT2(38,2,ent->FragmentIndex(),0);
  CHECK_INT2(38,3,ent->ConstType(),Float64);

  // 39: Fragment check
  frag = d->Fragment(0);
  CHECK_OK(39);
  CHECK_STRING(39,frag->Name(),__TEST__ "dirfile/format");

  // 40: Dirfile::NFragments check
  n = d->NFragments();
  CHECK_OK(40);
  CHECK_INT(40,n,1);

  // 41: Dirfile::Include check
  n = d->Include("form2");
  CHECK_OK2(41,1);
  CHECK_INT2(41,1,n,1);
  n = d->GetConstant("const2", Int8, &sc);
  CHECK_OK2(41,2);
  CHECK_INT2(41,2,sc,-19);

  // 42: Dirfile::NFieldsByType check
  n = d->NFieldsByType(LincomEntryType);
  CHECK_OK(42);
  CHECK_INT(42,n,3);

  // 43: Dirfile::FieldListByType check
  fields[0] = (char*)"lincom";
  fields[1] = (char*)"new2";
  fields[2] = (char*)"new3";
  list = d->FieldListByType(LincomEntryType);
  CHECK_OK(43);
  CHECK_STRING_ARRAY(43,n,list[i],fields[i]);

  // 44: Dirfile::NVectors check
  n = d->NVectors();
  CHECK_OK(44);
  CHECK_INT(44,n,19);

  // 45: Dirfile::VectorList check
  fields[0] = (char*)"INDEX";
  fields[1] = (char*)"bit";
  fields[2] = (char*)"data";
  fields[3] = (char*)"lincom";
  fields[4] = (char*)"linterp";
  fields[5] = (char*)"mult";
  fields[6] = (char*)"new1";
  fields[7] = (char*)"new10";
  fields[8] = (char*)"new2";
  fields[9] = (char*)"new3";
  fields[10] = (char*)"new4";
  fields[11] = (char*)"new5";
  fields[12] = (char*)"new6";
  fields[13] = (char*)"new7";
  fields[14] = (char*)"new8";
  fields[15] = (char*)"new9";
  fields[16] = (char*)"phase";
  fields[17] = (char*)"polynom";
  fields[18] = (char*)"sbit";
  fields[19] = (char*)"string";
  list = d->VectorList();
  CHECK_OK(45);
  CHECK_STRING_ARRAY(45,n,list[i],fields[i]);

  // 126: Dirfile::MAdd check
  q[0] = 9.9;
  q[1] = 8.8;
  q[2] = 7.7;
  q[3] = 6.6;
  lent.Dissociate();
  lent.SetName("mnew1");
  lent.SetNFields(2);
  lent.SetInput("in1", 0);
  lent.SetScale(q[0], 0);
  lent.SetOffset(q[1], 0);
  lent.SetInput("in2", 1);
  lent.SetScale(q[2], 1);
  lent.SetOffset(q[3], 1);
  d->MAdd(lent, "data");
  CHECK_OK2(126,1);

  ent = d->Entry("data/mnew1");
  CHECK_OK2(126,2);
  CHECK_INT2(126,1,ent->Type(),LincomEntryType);
  CHECK_INT2(126,2,ent->NFields(),2);
  CHECK_INT2(126,3,ent->FragmentIndex(),0);
  CHECK_STRING2(126,4,ent->Input(0),"in1");
  CHECK_STRING2(126,5,ent->Input(1),"in2");
  CHECK_INT2(126,6,ent->ComplexScalars(),0);
  CHECK_DOUBLE_ARRAY(126,7,2,ent->Scale(i),q[i * 2]);
  CHECK_DOUBLE_ARRAY(126,8,2,ent->Offset(i),q[i * 2 + 1]);

  // 56: Dirfile::GetString check
  n = d->GetString("string", GD_MAX_LINE_LENGTH, buf);
  CHECK_OK(56);
  CHECK_INT(56,n,18);
  CHECK_STRING(56,buf,"Zaphod Beeblebrox");

  // 57: Dirfile::Add / StringEntry check
  gent.SetName("new12");
  gent.SetFragmentIndex(0);
  d->Add(gent);
  CHECK_OK2(57,1);

  ent = d->Entry("new12");
  CHECK_OK2(57,2);
  CHECK_INT2(57,1,ent->Type(),StringEntryType);
  CHECK_INT2(57,2,ent->FragmentIndex(),0);

  n = d->GetString("new12", GD_MAX_LINE_LENGTH, buf);
  CHECK_OK2(57,3);
  CHECK_INT(57,n,1);
  CHECK_STRING(57,buf,"");

  // 59: Dirfile::AddSpec check
  d->AddSpec("lorem STRING \"Lorem ipsum\"", 0);
  CHECK_OK2(59,1);

  n = d->GetString("lorem", GD_MAX_LINE_LENGTH, buf);
  CHECK_OK2(59,2);
  CHECK_INT(59,n,12);
  CHECK_STRING(59,buf,"Lorem ipsum");

  // 60: Dirfile::MAddSpec check
  d->MAddSpec("ipsum STRING \"dolor sit amet.\"", "lorem");
  CHECK_OK2(60,1);

  n = d->GetString("lorem/ipsum", GD_MAX_LINE_LENGTH, buf);
  CHECK_OK2(60,2);
  CHECK_INT(60,n,16);
  CHECK_STRING(60,buf,"dolor sit amet.");

  // 61: Dirfile::PutConstant check
  sc = 61;
  n = d->PutConstant("const", Int8, &sc);
  CHECK_OK2(61,1);
  CHECK_INT2(61,1,n,0);

  n = d->GetConstant("const", Float32, &fl);
  CHECK_OK2(61,2);
  CHECK_INT2(61,2,n,0);
  CHECK_DOUBLE2(61,3,fl,61);

  // 62: Dirfile::PutString check
  n = d->PutString("string", "Arthur Dent");
  CHECK_OK2(62,1);
  CHECK_INT2(62,1,n,12);

  n = d->GetString("string", GD_MAX_LINE_LENGTH, buf);
  CHECK_OK2(62,2);
  CHECK_INT2(62,2,n,12);
  CHECK_STRING(62,buf,"Arthur Dent");

  // 63: Dirfile::NMFieldsByType check
  n = d->NMFieldsByType("data", LincomEntryType);
  CHECK_OK(63);
  CHECK_INT(63,n,1);

  // 64: Dirfile::MFieldListByType check
  fields[0] = (char*)"mnew1";
  list = d->MFieldListByType("data", LincomEntryType);
  CHECK_OK(64);
  CHECK_STRING_ARRAY(64,n,list[i],fields[i]);

  // 65: Dirfile::NMVectors check
  n = d->NMVectors("data");
  CHECK_OK(65);
  CHECK_INT(65,n,2);

  // 66: Dirfile::MVectorList check
  fields[0] = (char*)"mlut";
  fields[1] = (char*)"mnew1";
  list = d->MVectorList("data");
  CHECK_OK(66);
  CHECK_STRING_ARRAY(66,n,list[i],fields[i]);

  // 67: RawEntry check
  rep = reinterpret_cast<RawEntry*>(d->Entry("new1"));
  CHECK_OK2(67,1);
  rep->SetType(Int32,0);
  CHECK_OK2(67,2);
  rep->SetSamplesPerFrame(4,0);
  CHECK_OK2(67,3);

  ent = d->Entry("new1");
  CHECK_OK2(67,4);
  CHECK_INT2(67,1,ent->Type(),RawEntryType);
  CHECK_INT2(67,2,ent->FragmentIndex(),0);
  CHECK_INT2(67,3,ent->SamplesPerFrame(),4);
  CHECK_INT2(67,4,ent->RawType(),Int32);

  // 68: LincomEntry check
  lep = reinterpret_cast<LincomEntry*>(d->Entry("new2"));
  CHECK_OK2(68,1);
  lep->SetNFields(3);
  CHECK_OK2(68,2);
  lep->SetInput("in4",2);
  CHECK_OK2(68,3);
  lep->SetScale(1.96,2);
  CHECK_OK2(68,4);
  lep->SetOffset(0.22,2);
  CHECK_OK2(68,5);

  q[0] = 9.9;
  q[1] = 8.8;
  q[2] = 7.7;
  q[3] = 6.6;
  q[4] = 1.96;
  q[5] = 0.22;
  ent = d->Entry("new2");
  CHECK_OK2(68,6);
  CHECK_INT2(68,1,ent->Type(),LincomEntryType);
  CHECK_INT2(68,2,ent->NFields(),3);
  CHECK_INT2(68,3,ent->FragmentIndex(),0);
  CHECK_STRING2(68,4,ent->Input(0),"in1");
  CHECK_STRING2(68,5,ent->Input(1),"in2");
  CHECK_STRING2(68,6,ent->Input(2),"in4");
  CHECK_INT2(68,7,ent->ComplexScalars(),0);
  CHECK_DOUBLE_ARRAY(68,8,3,ent->Scale(i),q[i * 2]);
  CHECK_DOUBLE_ARRAY(68,9,3,ent->Offset(i),q[i * 2 + 1]);

  // 70: PolynomEntry check
  yep = reinterpret_cast<PolynomEntry*>(d->Entry("new4"));
  CHECK_OK2(70,1);
  yep->SetInput("in4");
  CHECK_OK2(70,2);
  yep->SetPolyOrd(4);
  CHECK_OK2(70,3);
  yep->SetCoefficient(55.5,4);
  CHECK_OK2(70,4);

  q[0] = 3.9;
  q[1] = 4.8;
  q[2] = 5.7;
  q[3] = 6.6;
  q[4] = 55.5;
  ent = d->Entry("new4");
  CHECK_OK2(70,5);
  CHECK_INT2(70,1,ent->Type(),PolynomEntryType);
  CHECK_INT2(70,2,ent->PolyOrd(),4);
  CHECK_INT2(70,3,ent->FragmentIndex(),0);
  CHECK_STRING2(70,4,ent->Input(),"in4");
  CHECK_INT2(70,5,ent->ComplexScalars(),0);
  CHECK_DOUBLE_ARRAY(70,6,5,ent->Coefficient(i),q[i]);

  // 72: LinterpEntry check
  nep = reinterpret_cast<LinterpEntry*>(d->Entry("new6"));
  CHECK_OK2(72,1);
  nep->SetInput("in3");
  CHECK_OK2(72,2);
  nep->SetTable("./other/table");
  CHECK_OK2(72,3);

  ent = d->Entry("new6");
  CHECK_OK2(72,2);
  CHECK_INT2(72,1,ent->Type(),LinterpEntryType);
  CHECK_INT2(72,2,ent->FragmentIndex(),0);
  CHECK_STRING2(72,3,ent->Input(),"in3");
  CHECK_STRING2(72,4,ent->Table(),"./other/table");

  // 73: BitEntry check
  bep = reinterpret_cast<BitEntry*>(d->Entry("new7"));
  CHECK_OK2(73,1);
  bep->SetInput("in3");
  CHECK_OK2(73,2);
  bep->SetFirstBit(3);
  CHECK_OK2(73,3);
  bep->SetNumBits(2);
  CHECK_OK2(73,4);

  ent = d->Entry("new7");
  CHECK_OK(73);
  CHECK_INT2(73,1,ent->Type(),BitEntryType);
  CHECK_INT2(73,2,ent->FragmentIndex(),0);
  CHECK_STRING2(73,3,ent->Input(),"in3");
  CHECK_INT2(73,4,ent->NumBits(),2);
  CHECK_INT2(73,5,ent->FirstBit(),3);

  // 74: SBitEntry check
  sep = reinterpret_cast<SBitEntry*>(d->Entry("new8"));
  CHECK_OK2(74,1);
  sep->SetInput("in4");
  CHECK_OK2(74,2);
  sep->SetFirstBit(1);
  CHECK_OK2(74,3);
  sep->SetNumBits(22);
  CHECK_OK2(74,4);

  ent = d->Entry("new8");
  CHECK_OK(74);
  CHECK_INT2(74,1,ent->Type(),SBitEntryType);
  CHECK_INT2(74,2,ent->FragmentIndex(),0);
  CHECK_STRING2(74,3,ent->Input(),"in4");
  CHECK_INT2(74,4,ent->NumBits(),22);
  CHECK_INT2(74,5,ent->FirstBit(),1);

  // 75: MultiplyEntry check
  mep = reinterpret_cast<MultiplyEntry*>(d->Entry("new9"));
  CHECK_OK2(75,1);
  mep->SetInput("in4",0);
  CHECK_OK2(75,2);
  mep->SetInput("in5",1);
  CHECK_OK2(75,3);

  ent = d->Entry("new9");
  CHECK_OK2(75,2);
  CHECK_INT2(75,1,ent->Type(),MultiplyEntryType);
  CHECK_INT2(75,2,ent->FragmentIndex(),0);
  CHECK_STRING2(75,3,ent->Input(0),"in4");
  CHECK_STRING2(75,4,ent->Input(1),"in5");

  // 76: PhsaeEntry check
  pep = reinterpret_cast<PhaseEntry*>(d->Entry("new10"));
  CHECK_OK2(76,1);
  pep->SetInput("in2");
  CHECK_OK2(76,2);
  pep->SetShift(8);
  CHECK_OK2(76,3);

  ent = d->Entry("new10");
  CHECK_OK(76);
  CHECK_INT2(76,1,ent->Type(),PhaseEntryType);
  CHECK_INT2(76,2,ent->FragmentIndex(),0);
  CHECK_STRING2(76,3,ent->Input(),"in2");
  CHECK_INT2(76,4,ent->Shift(),8);

  // 77: ConstEntry check
  cep = reinterpret_cast<ConstEntry*>(d->Entry("new11"));
  CHECK_OK2(76,1);
  cep->SetType(Float32);
  CHECK_OK2(76,2);

  ent = d->Entry("new11");
  CHECK_OK2(77,2);
  CHECK_INT2(77,1,ent->Type(),ConstEntryType);
  CHECK_INT2(77,2,ent->FragmentIndex(),0);
  CHECK_INT2(77,3,ent->ConstType(),Float32);

  // 78: Fragment::Encoding check
  frag = d->Fragment(0);
  CHECK_OK(78);
  CHECK_INT(78,frag->Encoding(),RawEncoding);

  // 79: Fragment::Endianness check
  CHECK_INT(79,frag->Endianness(),GD_LITTLE_ENDIAN);

  // 80: Dirfile::Name check
  str = d->Name();
  CHECK_OK(80);
  CHECK_STRING(80,str,__TEST__ "dirfile");

  // 81: Fragment::Parent check
  frag = d->Fragment(1);
  CHECK_OK(81);
  CHECK_INT(81,frag->Parent(),0);

  // 82: Fragment::SetProtection check
  frag->SetProtection(GD_PROTECT_DATA);
  CHECK_OK(82);

  // 83: Fragment::Protection check
  frag = d->Fragment(1);
  CHECK_OK(83);
  CHECK_INT(83,frag->Protection(),GD_PROTECT_DATA);

  // 84: RawEntry::FileName check
  str = rep->FileName();
  CHECK_OK(84);
  CHECK_STRING(84,str,__TEST__ "dirfile/new1");

  // 85: Dirfile::Reference check
  rep = d->Reference("new1");
  CHECK_OK(85);
  CHECK_STRING(85,rep->Name(),"new1");

  // 135: Dirfile::ReferenceFilename check
  str = d->ReferenceFilename();
  CHECK_OK(135);
  CHECK_STRING(135,str,__TEST__ "dirfile/new1");
  
  // 87: Fragment::SetEncoding check
  frag->SetEncoding(SlimEncoding,0);
  CHECK_OK(87);
  CHECK_INT(87,frag->Encoding(),SlimEncoding);

  // 88: Fragment::SetEndianness check
  frag->SetEndianness(GD_BIG_ENDIAN,0);
  CHECK_OK(88);
  CHECK_INT(88,frag->Endianness(),GD_BIG_ENDIAN);

  // 89: Dirfile::AlterSpec check
  d->AlterSpec("new10 PHASE in1 3");
  CHECK_OK2(89,1);

  ent = d->Entry("new10");
  CHECK_OK2(89,2);
  CHECK_INT2(89,1,ent->Type(),PhaseEntryType);
  CHECK_INT2(89,2,ent->FragmentIndex(),0);
  CHECK_STRING2(89,3,ent->Input(),"in1");
  CHECK_INT2(89,4,ent->Shift(),3);

  //  90: Dirfile::Delete check
  d->Delete("new10", 0);
  CHECK_OK2(90,1);

  ent = d->Entry("new10");
  CHECK_ERROR2(90,2,GD_E_BAD_CODE);

  // 91: Dirfile::MAlterSpec check
  d->MAlterSpec("mnew1 LINCOM 2 in4 1 2 in5 3 4", "data", 0);
  CHECK_OK2(91,1);

  ent = d->Entry("data/mnew1");
  CHECK_OK2(91,2);
  CHECK_INT2(91,1,ent->Type(),LincomEntryType);
  CHECK_INT2(91,2,ent->NFields(),2);
  CHECK_INT2(91,3,ent->FragmentIndex(),0);
  CHECK_STRING2(91,4,ent->Input(0),"in4");
  CHECK_STRING2(91,5,ent->Input(1),"in5");
  CHECK_INT2(91,6,ent->ComplexScalars(),0);
  CHECK_DOUBLE_ARRAY(91,7,2,ent->Scale(i),i * 2 + 1);
  CHECK_DOUBLE_ARRAY(91,8,2,ent->Offset(i),i * 2 + 2);

  // 92: Entry::Move check
  ent = d->Entry("new9");
  CHECK_OK2(92,1);
  ent->Move(1,0);
  CHECK_OK2(92,2);
  CHECK_INT(92,ent->FragmentIndex(),1);

  // 93: Entry::Rename check
  ent->Rename("newer",0);
  CHECK_OK2(93,1);

  ent = d->Entry("new9");
  CHECK_ERROR2(93,2,GD_E_BAD_CODE);

  ent = d->Entry("newer");
  CHECK_OK2(93,3);
  CHECK_INT2(93,1,ent->Type(),MultiplyEntryType);
  CHECK_INT2(93,2,ent->FragmentIndex(),1);
  CHECK_STRING2(93,3,ent->Input(0),"in4");
  CHECK_STRING2(93,4,ent->Input(1),"in5");

  // 94: Dirfile::UnInclude check
  d->UnInclude(1,0);
  CHECK_OK2(94,1);

  ent = d->Entry("newer");
  CHECK_ERROR2(94,2,GD_E_BAD_CODE);

  // 95: Fragment::FrameOffset check
  frag = d->Fragment(0);
  CHECK_OK(95);
  CHECK_INT(95,frag->FrameOffset(),0);

  // 96: Fragment::SetFrameOffset check
  frag->SetFrameOffset(33,0);
  CHECK_OK(96);
  CHECK_INT(96,frag->FrameOffset(),33);

  // 97: Dirfile::NativeType check
  n = d->NativeType("data");
  CHECK_OK(97);
  CHECK_INT(97,n,Int8);

  // 99: Dirfile::Validate check
  n = d->Validate("new7");
  CHECK_ERROR(99,GD_E_BAD_CODE);
  CHECK_INT(99,n,-1);

  // 101: Dirfile::FrameNum check
  d->Reference("data");
  dp = d->FrameNum("data", 33.3, 6);
  CHECK_OK(101);
  CHECK_DOUBLE(101,dp,37.0375);
  
  delete d;
  unlink(data);
  unlink(new1);
  unlink(format);
  unlink(form2);
  rmdir(filedir);

  if (ne) {
    cerr << "ne = " << ne << endl;
    return 1;
  }

  return 0;
}
