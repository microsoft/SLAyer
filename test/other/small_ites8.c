/*
  Copyright (c) Microsoft Corporation.  All rights reserved.

  Many small ite-s. (Taken from SLAyer/test/T2/Skin.c.)
  Really hits SLAyer perf.
*/

// Initialization macros
#define GRANULARITY 3

#define INIT_VAR(V)				\
  if (V > GRANULARITY) { V = GRANULARITY; }	\
  else if (V < 0) { V = 0; }


int main() {

  // Variables.
  int wntext0, wntext0_new;
  int frizzled0, frizzled0_new;
  int dsh0, dsh0_new;
  int axin0, axin0_new;
  int bcat0, bcat0_new;
  int gt1_0, gt1_0_new;
  int gt2_0, gt2_0_new;
  int delta0, delta0_new;
  int deltaext0, deltaext0_new;
  int notchic0, notchic0_new;
  int p21_0, p21_0_new;
  int wnt0, wnt0_new;

  int wntext1, wntext1_new;
  int frizzled1, frizzled1_new;
  int dsh1, dsh1_new;
  int axin1, axin1_new;
  int bcat1, bcat1_new;
  int gt1_1, gt1_1_new;
  int gt2_1, gt2_1_new;
  int delta1, delta1_new;
  int deltaext1, deltaext1_new;
  int notchic1, notchic1_new;
  int p21_1, p21_1_new;
  int wnt1, wnt1_new;

  int wntext2, wntext2_new;
  int frizzled2, frizzled2_new;
  int dsh2, dsh2_new;
  int axin2, axin2_new;
  int bcat2, bcat2_new;
  int gt1_2, gt1_2_new;
  int gt2_2, gt2_2_new;
  int delta2, delta2_new;
  int deltaext2, deltaext2_new;
  int notchic2, notchic2_new;
  int p21_2, p21_2_new;
  int wnt2, wnt2_new;

  int wntext3, wntext3_new;
  int frizzled3, frizzled3_new;
  int dsh3, dsh3_new;
  int axin3, axin3_new;
  int bcat3, bcat3_new;
  int gt1_3, gt1_3_new;
  int gt2_3, gt2_3_new;
  int delta3, delta3_new;
  int deltaext3, deltaext3_new;
  int notchic3, notchic3_new;
  int p21_3, p21_3_new;
  int wnt3, wnt3_new;

  int wntext4, wntext4_new;
  int frizzled4, frizzled4_new;
  int dsh4, dsh4_new;
  int axin4, axin4_new;
  int bcat4, bcat4_new;
  int gt1_4, gt1_4_new;
  int gt2_4, gt2_4_new;
  int delta4, delta4_new;
  int deltaext4, deltaext4_new;
  int notchic4, notchic4_new;
  int p21_4, p21_4_new;
  int wnt4, wnt4_new;

  // Initialization
/*   INIT_WNTEXT(wntext0) ; */
  INIT_VAR(wntext0) ;
  INIT_VAR(frizzled0) ;
  INIT_VAR(dsh0) ;
  INIT_VAR(axin0) ;
  INIT_VAR(bcat0) ;
  INIT_VAR(gt1_0) ;
  INIT_VAR(gt2_0) ;
  INIT_VAR(delta0) ;
/*   INIT_VAR(deltaext0) ; */
/*   INIT_VAR(notchic0) ; */
/*   INIT_VAR(p21_0) ; */
/*   INIT_VAR(wnt0) ; */
/*   INIT_VAR(wntext1) ; */
/*   INIT_VAR(frizzled1) ; */
/*   INIT_VAR(dsh1) ; */
/*   INIT_VAR(axin1) ; */
/*   INIT_VAR(bcat1) ; */
/*   INIT_VAR(gt1_1) ; */
/*   INIT_VAR(gt2_1) ; */
/*   INIT_VAR(delta1) ; */
/*   INIT_VAR(deltaext1) ; */
/*   INIT_VAR(notchic1) ; */
/*   INIT_VAR(p21_1) ; */
/*   INIT_VAR(wnt1) ; */
/*   INIT_VAR(wntext2) ; */
/*   INIT_VAR(frizzled2) ; */
/*   INIT_VAR(dsh2) ; */
/*   INIT_VAR(axin2) ; */
/*   INIT_VAR(bcat2) ; */
/*   INIT_VAR(gt1_2) ; */
/*   INIT_VAR(gt2_2) ; */
/*   INIT_VAR(delta2) ; */
/*   INIT_VAR(deltaext2) ; */
/*   INIT_VAR(notchic2) ; */
/*   INIT_VAR(p21_2) ; */
/*   INIT_VAR(wnt2) ; */
/*   INIT_VAR(wntext3) ; */
/*   INIT_VAR(frizzled3) ; */
/*   INIT_VAR(dsh3) ; */
/*   INIT_VAR(axin3) ; */
/*   INIT_VAR(bcat3) ; */
/*   INIT_VAR(gt1_3) ; */
/*   INIT_VAR(gt2_3) ; */
/*   INIT_VAR(delta3) ; */
/*   INIT_VAR(deltaext3) ; */
/*   INIT_VAR(notchic3) ; */
/*   INIT_VAR(p21_3) ; */
/*   INIT_VAR(wnt3) ; */
/*   INIT_VAR(wntext4) ; */
/*   INIT_VAR(frizzled4) ; */
/*   INIT_VAR(dsh4) ; */
/*   INIT_VAR(axin4) ; */
/*   INIT_VAR(bcat4) ; */
/*   INIT_VAR(gt1_4) ; */
/*   INIT_VAR(gt2_4) ; */
/*   INIT_VAR(delta4) ; */
/*   INIT_VAR(deltaext4) ; */
/*   INIT_VAR(notchic4) ; */
/*   INIT_VAR(p21_4) ; */
/*   INIT_VAR(wnt4) ; */

  return 0;
}
