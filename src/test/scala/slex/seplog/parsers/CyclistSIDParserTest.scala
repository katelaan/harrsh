package slex.seplog.parsers

import slex.test.{SlexTableTest}

/**
  * Created by jkatelaa on 10/20/16.
  */
class CyclistSIDParserTest extends SlexTableTest {

  import CyclistSIDParser._

  val Success = true
  val Failure = false

  val inputs = Table(
    ("parser", "input", "result"),
    (parseAtomSeq, "a=f * I1355_1(a,b,c,d,e,f)", Success),
    (parseAtomSeq, "a!=f * I1355_1(a,b,c,d,e,f)", Success),
    (parseAtomSeq, "nil!=e * e->a' * I1317_1(a,b,c,d,e,a')", Success),
    (parseRule, "nil!=d * d->a' * I166_1(a,b,c,d,a') => I046(a,b,c,d)", Success),
    // A couple of representative examples
    (parseSID, fullExample, Success),
    (parseSID, fullExample2, Success),
    (parseSID, fullExample3, Success)
  )

  property ("The Cyclist SID parser should work") {
    forAll(inputs) {
      (parser, input, expectedResult) =>
        val parseResult = parseAll(parser, input)

        info(""+parseResult)

        if (expectedResult) {
          parseResult.successful should be (expectedResult)
        }
    }
  }


  private def fullExample = """ls {
    nil=a => ls(a) |
      nil!=a * I001_1(a) => ls(a)
  } ;

  I209166 {
    I40239_0(a,b,c,d,e,f,g,j) => I209166(a,b,c,d,e,f,g,h,i,j)
  } ;

  I209106 {
    nil!=i * i->a' * I209166_1(a,b,c,d,e,f,g,h,i,a') => I209106(a,b,c,d,e,f,g,h,i)
  } ;

  I209107 {
    I40306_0(a,b,c,d,e,f,g,i) => I209107(a,b,c,d,e,f,g,h,i)
  } ;

  I209073 {
    nil=i * I209107_1(a,b,c,d,e,f,g,h,i) => I209073(a,b,c,d,e,f,g,h,i) |
      nil!=i * I209106_1(a,b,c,d,e,f,g,h,i) => I209073(a,b,c,d,e,f,g,h,i)
  } ;

  I40305 {
    nil!=h * h->a' * I209073_1(a,b,c,d,e,f,g,h,a') => I40305(a,b,c,d,e,f,g,h)
  } ;

  I60989 {
    I40306_0(i,b,c,d,e,f,g,h) => I60989(a,b,c,d,e,f,g,h,i)
  } ;

  I40341 {
    nil!=a * a->a' * I60989_1(a,b,c,d,e,f,g,h,a') => I40341(a,b,c,d,e,f,g,h)
  } ;

  I40306 {
    nil=a => I40306(a,b,c,d,e,f,g,h) |
      nil!=a * I40341_1(a,b,c,d,e,f,g,h) => I40306(a,b,c,d,e,f,g,h)
  } ;

  I40239 {
    nil=h * I40306_1(a,b,c,d,e,f,g,h) => I40239(a,b,c,d,e,f,g,h) |
      nil!=h * I40305_1(a,b,c,d,e,f,g,h) => I40239(a,b,c,d,e,f,g,h)
  } ;

  I40109 {
    nil!=g * g->a' * I40239_1(a,b,c,d,e,f,g,a') => I40109(a,b,c,d,e,f,g)
  } ;

  I40192 {
    I40110_0(h,b,c,d,e,f,g) => I40192(a,b,c,d,e,f,g,h)
  } ;

  I40179 {
    nil!=a * a->a' * I40192_1(a,b,c,d,e,f,g,a') => I40179(a,b,c,d,e,f,g)
  } ;

  I40110 {
    a=g => I40110(a,b,c,d,e,f,g) |
      a!=g * I40179_1(a,b,c,d,e,f,g) => I40110(a,b,c,d,e,f,g)
  } ;

  I40085 {
    nil=g * I40110_1(a,b,c,d,e,f,g) => I40085(a,b,c,d,e,f,g) |
      nil!=g * I40109_1(a,b,c,d,e,f,g) => I40085(a,b,c,d,e,f,g)
  } ;

  I39957 {
    nil!=f * f->a' * I40085_1(a,b,c,d,e,f,a') => I39957(a,b,c,d,e,f)
  } ;

  I40046 {
    I39958_0(a,g,c,d,e,f) => I40046(a,b,c,d,e,f,g)
  } ;

  I40035 {
    nil!=b * b->a' * I40046_1(a,b,c,d,e,f,a') => I40035(a,b,c,d,e,f)
  } ;

  I39958 {
    b=f => I39958(a,b,c,d,e,f) |
      b!=f * I40035_1(a,b,c,d,e,f) => I39958(a,b,c,d,e,f)
  } ;

  I39937 {
    nil=f * I39958_1(a,b,c,d,e,f) => I39937(a,b,c,d,e,f) |
      nil!=f * I39957_1(a,b,c,d,e,f) => I39937(a,b,c,d,e,f)
  } ;

  I182 {
    nil!=e * e->a' * I39937_1(a,b,c,d,e,a') => I182(a,b,c,d,e)
  } ;

  I11677 {
    I183_0(a,b,f,d,e) => I11677(a,b,c,d,e,f)
  } ;

  I7185 {
    nil!=c * c->a' * I11677_1(a,b,c,d,e,a') => I7185(a,b,c,d,e)
  } ;

  I183 {
    c=e => I183(a,b,c,d,e) |
      c!=e * I7185_1(a,b,c,d,e) => I183(a,b,c,d,e)
  } ;

  I166 {
    nil=e * I183_1(a,b,c,d,e) => I166(a,b,c,d,e) |
      nil!=e * I182_1(a,b,c,d,e) => I166(a,b,c,d,e)
  } ;

  I046 {
    nil!=d * d->a' * I166_1(a,b,c,d,a') => I046(a,b,c,d)
  } ;

  I063 {
    I047_0(e,b,c,d) => I063(a,b,c,d,e)
  } ;

  I056 {
    nil!=a * a->a' * I063_1(a,b,c,d,a') => I056(a,b,c,d)
  } ;

  I047 {
    nil=a => I047(a,b,c,d) |
      nil!=a * I056_1(a,b,c,d) => I047(a,b,c,d)
  } ;

  I034 {
    nil=d * I047_1(a,b,c,d) => I034(a,b,c,d) |
      nil!=d * I046_1(a,b,c,d) => I034(a,b,c,d)
  } ;

  I021 {
    nil!=c * c->a' * I034_1(a,b,c,a') => I021(a,b,c)
  } ;

  I022 {
    I008_0(b,c) => I022(a,b,c)
  } ;

  I013 {
    nil=c * I022_1(a,b,c) => I013(a,b,c) |
      nil!=c * I021_1(a,b,c) => I013(a,b,c)
  } ;

  I007 {
    nil!=b * b->a' * I013_1(a,b,a') => I007(a,b)
  } ;

  I008 {
    emp => I008(a,b)
  } ;

  I003 {
    nil=b * I008_1(a,b) => I003(a,b) |
      nil!=b * I007_1(a,b) => I003(a,b)
  } ;

  I001 {
    nil!=a * a->a' * I003_1(a,a') => I001(a)
  }"""

  def fullExample2 =
    """
      |BinListFirst {
      |  emp => BinListFirst(x) |
      |  nil!=x * x->yp',xp' * BinListFirst(yp') => BinListFirst(x)
      |} ;
      |BinListSecond {
      |  emp => BinListSecond(x) |
      |  nil!=x * x->yp',xp' * BinListSecond(xp') => BinListSecond(x)
      |} ;
      |BinPath {
      |  x=y => BinPath(x,y) |
      |  nil!=x * x->xp',yp' * BinPath(xp',y) => BinPath(x,y) |
      |  nil!=x * x->xp',yp' * BinPath(yp',y) => BinPath(x,y)
      |} ;
      |BinTree {
      |  emp => BinTree(x) |
      |  nil!=x * x->yp',xp' * BinTree(yp') * BinTree(xp') => BinTree(x)
      |} ;
      |BinTreeSeg {
      |  x=y => BinTreeSeg(x,y) |
      |  nil!=x * x->xp',yp' * BinTreeSeg(xp',y) * BinTree(yp') => BinTreeSeg(x,y) |
      |  nil!=x * x->xp',yp' * BinTree(xp') * BinTreeSeg(yp',y) => BinTreeSeg(x,y)
      |} ;
      |BSLL {
      |  x=y => BSLL(x,y) |
      |  nil!=xp' * xp'->yp',y * BSLL(x,xp') => BSLL(x,y)
      |} ;
      |clist {
      |  in=self_19' * in->p_18' * lseg(p_18',self_19') => clist(in)
      |} ;
      |DLL1_plus {
      |  hd->nil,p => DLL1_plus(hd,p) |
      |  hd->x',p * DLL1_plus(x',hd) => DLL1_plus(hd,p)
      |} ;
      |DLL2_plus {
      |  hd=tl * hd->n,p,down_hd' * DLL1_plus(down_hd',hd) => DLL2_plus(hd,p,tl,n) |
      |  hd->x',p,down_hd' * DLL1_plus(down_hd',hd) * DLL2_plus(x',hd,tl,n) => DLL2_plus(hd,p,tl,n)
      |} ;
      |DLL2_plus_rev {
      |  hd=tl * hd->n,p,down_hd' * DLL1_plus(down_hd',hd) => DLL2_plus_rev(hd,p,tl,n) |
      |  tl->n,x',down_hd' * DLL1_plus(down_hd',tl) * DLL2_plus_rev(hd,p,x',tl) => DLL2_plus_rev(hd,p,tl,n)
      |} ;
      |DLL {
      |  p=tl * hd=n => DLL(hd,p,tl,n) |
      |  hd->x',p * DLL(x',hd,tl,n) => DLL(hd,p,tl,n)
      |} ;
      |dll_e1 {
      |  in=s' * q=p1' * dll(q1',s') * in->p1',q1' => dll_e1(in,q)
      |} ;
      |dll_e2 {
      |  n'=q1' * p1'=p2' * in=s' * q=p2' * in->p1',n' * dll(q1',s') => dll_e2(in,q)
      |} ;
      |dll_e3 {
      |  p=q' * dll(in,q') => dll_e3(in,p)
      |} ;
      |DLL_plus {
      |  hd=tl * hd->n,p => DLL_plus(hd,p,tl,n) |
      |  hd->x',p * DLL_plus(x',hd,tl,n) => DLL_plus(hd,p,tl,n)
      |} ;
      |DLL_plus_mid {
      |  hd=tl * hd->n,p => DLL_plus_mid(hd,p,tl,n) |
      |  hd->tl,p * points_to(tl,n,hd) => DLL_plus_mid(hd,p,tl,n) |
      |  x'->y',z' * DLL_plus(y',x',tl,n) * DLL_plus_rev(hd,p,z',x') => DLL_plus_mid(hd,p,tl,n)
      |} ;
      |DLL_plus_rev {
      |  hd=tl * hd->n,p => DLL_plus_rev(hd,p,tl,n) |
      |  tl->n,x' * DLL_plus_rev(hd,p,x',tl) => DLL_plus_rev(hd,p,tl,n)
      |} ;
      |elseg {
      |  in=p => elseg(in,p) |
      |  in->a' * a'->b' * elseg(b',p) => elseg(in,p)
      |} ;
      |enode {
      |  p=p0' * l=l0' * r=r0' * n=n0' * in->p0',l0',r0',n0' => enode(in,p,l,r,n)
      |} ;
      |eright_nil {
      |  p0'=p1' * l0'=l1' * r0'=r1' * n0'=n1' * nil=r1' * in->p0',l0',r0',n0' => eright_nil(in)
      |} ;
      |eright_nnil {
      |  p0'=p1' * l0'=l1' * r0'=r1' * n0'=n1' * nil=r1' * in->p0',l0',r0',n0' * tree(l1') * tree(r1') => eright_nnil(in)
      |} ;
      |etll {
      |  p=p1' * t=t1' * tll(in,p1',r,t1') => etll(in,p,t,r)
      |} ;
      |List {
      |  nil!=x * x->y => List(x,y) |
      |  nil!=x * x->xp' * List(xp',y) => List(x,y)
      |} ;
      |ListE {
      |  nil!=x * x->xp' * ListO(xp',y) => ListE(x,y)
      |} ;
      |ListO {
      |  nil!=x * x->y => ListO(x,y) |
      |  nil!=x * x->xp' * ListE(xp',y) => ListO(x,y)
      |} ;
      |ListX {
      |  ListO(x,y) => ListX(x,y) |
      |  ListE(x,y) => ListX(x,y)
      |} ;
      |ll {
      |  nil=in => ll(in) |
      |  in->q_18' * ll(q_18') => ll(in)
      |} ;
      |ll_e1 {
      |  in->q' * ll(q') => ll_e1(in)
      |} ;
      |ll_e2 {
      |  p'=q' * in->p' * ll(q') => ll_e2(in)
      |} ;
      |ls {
      |  x=y => ls(x,y) |
      |  nil!=x * x!=y * x->xp' * ls(xp',y) => ls(x,y)
      |} ;
      |lseg {
      |  in=p => lseg(in,p) |
      |  in->a' * lseg(a',p) => lseg(in,p)
      |} ;
      |lseg_e1 {
      |  p=q' * lseg(in,p) => lseg_e1(in,p)
      |} ;
      |lso {
      |  in=out => lso(in,out) |
      |  in!=out * in->u' * lso(u',out) => lso(in,out)
      |} ;
      |lsso {
      |  in=out => lsso(in,out) |
      |  in->u',u' * lsso(u',out) => lsso(in,out)
      |} ;
      |ltll {
      |  in->p,l,r,D * tll(l,in,v,l1') * tll(r,in,l1',t) => ltll(in,p,l,r,D,v,t)
      |} ;
      |nll {
      |  in=out => nll(in,out,boundary) |
      |  in!=out * in->u',Z1' * lso(Z1',boundary) * nll(u',out,boundary) => nll(in,out,boundary)
      |} ;
      |node2_e1 {
      |  p=p1' * q=n1' * in->p1',n1' => node2_e1(in,p,q)
      |} ;
      |node_e1 {
      |  q=p' * in->p' => node_e1(in,q)
      |} ;
      |olseg {
      |  in->p => olseg(in,p) |
      |  in->a' * a'->b' * olseg(b',p) => olseg(in,p)
      |} ;
      |points_to {
      |  a->b,c => points_to(a,b,c)
      |} ;
      |right1 {
      |  lseg(in,u') * u'->p => right1(in,p)
      |} ;
      |right2 {
      |  lseg(in,u') * lseg(u',p) => right2(in,p)
      |} ;
      |right3 {
      |  lseg(in,u') * lseg(u',u2') * lseg(u2',p) => right3(in,p)
      |} ;
      |right4 {
      |  lseg(in,u') * lseg(u',w') => right4(in)
      |} ;
      |right5 {
      |  lseg(in,w') => right5(in)
      |} ;
      |right {
      |  elseg(in,u') * elseg(u',p) => right(in,p)
      |} ;
      |right_nil {
      |  nil=r' * in->p',l',r',n' => right_nil(in)
      |} ;
      |right_nnil {
      |  nil!=r' * in->p',l',r',n' * tree(l') * tree(r') => right_nnil(in)
      |} ;
      |RList {
      |  nil!=x * x->y => RList(x,y) |
      |  nil!=xp' * xp'->y * RList(x,xp') => RList(x,y)
      |} ;
      |skl1 {
      |  hd=ex => skl1(hd,ex) |
      |  hd!=ex * hd->nil,tl' * skl1(tl',ex) => skl1(hd,ex)
      |} ;
      |skl2 {
      |  hd=ex => skl2(hd,ex) |
      |  hd!=ex * hd->tl',Z1' * skl1(Z1',tl') * skl2(tl',ex) => skl2(hd,ex)
      |} ;
      |skl3 {
      |  hd=ex => skl3(hd,ex) |
      |  hd!=ex * hd->tl',Z2',Z1' * skl1(Z1',Z2') * skl2(Z2',tl') * skl3(tl',ex) => skl3(hd,ex)
      |} ;
      |SLL {
      |  x=y => SLL(x,y) |
      |  nil!=x * x->xp',yp' * SLL(xp',y) => SLL(x,y)
      |} ;
      |TLL_aux {
      |  x->back,r',up',nil * TLL_aux(up',p,lr',x,top,mright) * TLL_plus(r',x,z,lr') => TLL_aux(x,p,z,back,top,mright) |
      |  x=top * x->back,r',p,nil * TLL_plus(r',x,z,mright) => TLL_aux(x,p,z,back,top,mright)
      |} ;
      |tll {
      |  nil=l_23' * in=ll * lr=lr_28' * in->p_21',D1_22',l_23',lr_28' => tll(in,p,ll,lr) |
      |  nil!=r_25' * p=p_29' * in=self_30' * ll=ll_31' * in=self_32' * z_33'=z_27' * lr=lr_34' * in->p_29',l_24',r_25',D2_26' * tll(l_24',self_30',ll_31',z_27') * tll(r_25',self_32',z_33',lr_34') => tll(in,p,ll,lr)
      |} ;
      |TLL_plus {
      |  root=ll * root->nil,nil,par,lr => TLL_plus(root,par,ll,lr) |
      |  root->l',r',par,nil * TLL_plus(l',root,ll,z') * TLL_plus(r',root,z',lr) => TLL_plus(root,par,ll,lr)
      |} ;
      |TLL_plus_rev {
      |  top=mleft * top->nil,nil,p,mright => TLL_plus_rev(top,p,mleft,mright) |
      |  mleft=x' * x'->nil,nil,up',lr' * TLL_aux(up',p,lr',x',top,mright) => TLL_plus_rev(top,p,mleft,mright)
      |} ;
      |TLL_tail {
      |  root=ll * root=tr * root->nil,nil,par,lr => TLL_tail(root,par,ll,tr,lr) |
      |  root->l',r',par,nil * TLL_plus(l',root,ll,z') * TLL_tail(r',root,z',tr,lr) => TLL_tail(root,par,ll,tr,lr)
      |} ;
      |TPP_aux {
      |  x->down,right',up' * TPP_plus(right',x) * TPP_aux(up',x,top,b) => TPP_aux(x,down,top,b) |
      |  x->left',down,up' * TPP_plus(left',x) * TPP_aux(up',x,top,b) => TPP_aux(x,down,top,b) |
      |  x=top * x->down,right',b * TPP_plus(right',x) => TPP_aux(x,down,top,b) |
      |  x=top * x->left',down,b * TPP_plus(left',x) => TPP_aux(x,down,top,b)
      |} ;
      |TPP_plus {
      |  x->nil,nil,back => TPP_plus(x,back) |
      |  x->y',z',back * TPP_plus(y',x) * TPP_plus(z',x) => TPP_plus(x,back)
      |} ;
      |TPP_plus_rev {
      |  top->nil,nil,b => TPP_plus_rev(top,b) |
      |  x'->nil,nil,up' * TPP_aux(up',x',top,b) => TPP_plus_rev(top,b)
      |}
      |
    """.stripMargin

  def fullExample3 =
    """
      |p1 {
      |  nil!=x4 * x3!=x6 * x8->x5 * x2->x1 * p1(x4) * x7->nil => p1 |
      |  x6!=x7 * x2->x3 * p5(x5) * x1=nil * p5(x7) * p2(x5,x9,x10,x8) * x3->x6 * x2!=x1 => p1(x1)
      |} ;
      |
      |p2 {
      |  x1->x5 * p2(x4,x3,x6,x2) * x10!=x9 * p1(x7) * x8->nil * p1(x1) * x2!=x5 => p2(x1,x2,x3,x4) |
      |  x3=x8 * p3(x5,nil) * p5(x4) * x9->x8 * x3=x2 * p5(x1) * p2(x4,x8,x1,x6) => p2(x1,x2,x3,x4)
      |} ;
      |
      |p3 {
      |  nil=x6 * x3->x5 => p3(x1,x2) |
      |  x5=x1 * x10!=nil * x8!=x3 * p2(x7,x5,x2,x5) * p4(x9) * x3=x6 => p3(x1,x2) |
      |  p3(x5,x3) * nil!=x9 * x1!=x8 * p5(x2) * x7=x9 * nil=x4 * x5->x8 * x6!=x7 => p3(x1,x2)
      |} ;
      |
      |p4 {
      |  x3=x1 * x6->x9 * x7=x2 * x1->x4 * x5=nil * x2=x9 * x8->x2 * x1=x10 * p3(x3,x6) => p4(x1) |
      |  x6!=nil * x7=nil * nil!=x2 * x1=x8 * x6->nil * x1=x4 * x9!=x8 * x7->x5 => p4(x1)
      |} ;
      |
      |p5 {
      |  x8->x4 * x1=nil * x2->x3 => p5(x1) |
      |  nil!=x5 * x2->x4 * x3=x6 * x8=x9 => p5(x1) |
      |  x6->x3 * x2->nil * nil!=x5 * p4(x4) * x9!=x7 * x10->x5 * x10!=x5 * p1(x3) => p5(x1)
      |}
    """.stripMargin

}
