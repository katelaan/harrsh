package at.forsyte.harrsh.parsers

import at.forsyte.harrsh.test.HarrshTableTest
import at.forsyte.harrsh.parsers.SIDParsers.CyclistSIDParser

/**
  * Created by jkatelaa on 10/20/16.
  */
class CyclistSIDParserTest extends HarrshTableTest {

  val Success = true
  val Failure = false

  val inputs = Table(
    ("input", "result"),
    // A couple of representative examples
    (fullExample, Success),
    (fullExample2, Success),
    (fullExample3, Success),
    (fullExample4, Success),
    (fullExample5, Success),
    (fullExample6, Success)
  )

  property ("The Cyclist SID parser should work") {
    forAll(inputs) {
      (input, expectedResult) =>
        val parseResult = CyclistSIDParser.runOnSID(input)

        info(""+parseResult)

        if (expectedResult) {
          parseResult.isDefined should be (expectedResult)
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
      |  nil!=x4 * x3!=x6 * x8->x5 * x2->x1 * p1(x4) * x7->nil => p1(x1) |
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

  def fullExample4 =
  """
    |RList {
    |	x->y => RList(x,y) |
    |	RList(x,x') * x'->y => RList(x,y)
    |} ;
    |
    |List {
    |	x->y => List(x,y) |
    |	x->x' * List(x',y) => List(x,y)
    |} ;
    |
    |ListO {
    |	x->y => ListO(x,y) |
    |	x->x' * ListE(x',y) => ListO(x,y)
    |} ;
    |
    |ListE {
    |	x->x' * ListO(x',y) => ListE(x,y)
    |} ;
    |
    |PeList {
    |	x=y => PeList(x,y) |
    |	x->x' * PeList(x',y) => PeList(x,y)
    |} ;
    |
    |DLL {
    |	x=y * z=w => DLL(x,y,z,w) |
    |	x->z',w * DLL(z',y,z,x) => DLL(x,y,z,w)
    |} ;
    |
    |SLL {
    |	x=y => SLL(x,y) |
    |	x->x',y' * SLL(x',y) => SLL(x,y)
    |} ;
    |
    |BSLL {
    |	x=y => BSLL(x,y) |
    |	BSLL(x,x') * x'->y',y => BSLL(x,y)
    |} ;
    |
    |BinTree {
    |	emp => BinTree(x) |
    |	x->y',x' * BinTree(y') * BinTree(x') => BinTree(x)
    |} ;
    |
    |BinTreeSeg {
    |	x=y => BinTreeSeg(x,y) |
    |	x->x',y' * BinTreeSeg(x',y) * BinTree(y') => BinTreeSeg(x,y) |
    |	x->x',y' * BinTree(x') * BinTreeSeg(y',y) => BinTreeSeg(x,y)
    |} ;
    |
    |BinListFirst {
    |	emp => BinListFirst(x) |
    |	x->y',x' * BinListFirst(y') => BinListFirst(x)
    |} ;
    |
    |BinListSecond {
    |	emp => BinListSecond(x) |
    |	x->y',x' * BinListSecond(x') => BinListSecond(x)
    |} ;
    |
    |BinPath {
    |	x=y => BinPath(x,y) |
    |	x->x',y' * BinPath(x',y) => BinPath(x,y) |
    |	x->x',y' * BinPath(y',y) => BinPath(x,y)
    |} ;
    |
    |ls {
    |    x=y => ls(x,y) |
    |    x!=y * x->x' * ls(x',y) => ls(x,y)
    |} ;
    |
    |bt {
    |	x=nil => bt(x) |
    |	x->y',x' * bt(y') * bt(x') => bt(x)
    |} ;
    |
    |cls {
    |        x->y' * ls(y',x) => cls(x)
    |} ;
    |
    |dls {
    |        x=y => dls(x,y) |
    |        x!=y * x-> y',x' * dls(x',y) => dls(x,y)
    |} ;
    |
    |lsls {
    |  x=nil => lsls(x) |
    |  x->y',x' * lsls(y') * dls(x',nil) => lsls(x)
    |} ;
    |
    |lsbt {
    |  x=nil => lsbt(x) |
    |  x->y',x' * lsbt(x') * bt(y') => lsbt(x)
    |} ;
    |
    |spTrue {
    |	emp => spTrue() |
    |	x'->y' * spTrue() => spTrue()
    |};
    |
    |spTrue2 {
    |	emp => spTrue2() |
    |	x'->y',z' * spTrue2() => spTrue2()
    |}
  """.stripMargin

  def fullExample5 =
    """
      |P {
      |  one(x1) * one(x2) * one(x3) * one(x4) * one(x5) * one(x6) * one(x7) * one(x8) * one(x9) * one(x10) * one(x11) * Q(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11) => P(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11)
      |} ;
      |
      |Q {
      |  zero(y1) * zero(y2) * zero(y3) * zero(y4) * zero(y5) * zero(y6) * zero(y7) * zero(y8) * zero(y9) * zero(y10) * zero(y11) => Q(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11) |
      |  succ11circuit(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11) * Q(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11) => Q(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11)
      |} ;
      |
      |succ11circuit {
      |  not(x1,y1) * xor(x1,x2,y2) * and(x1,x2,z3) * xor(z3,x3,y3) * and(z3,x3,z4) * xor(x4,y4,z4) * and(z4,x4,z5) * xor(x5,y5,z5) * and(z5,x5,z6) * xor(x6,y6,z6) * and(z6,x6,z7) * xor(x7,y7,z7) * and(z7,x7,z8) * xor(x8,y8,z8) * and(z8,x8,z9) * xor(x9,y9,z9) * and(z9,x9,z10) * xor(x10,y10,z10) * and(z10,x10,z11) * xor(x11,y11,z11) => succ11circuit(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11)
      |} ;
      |
      |not {
      |  zero(x) * one(y) => not(x,y) |
      |  one(x) * zero(y) => not(x,y)
      |} ;
      |
      |xor {
      |  zero(x) * zero(y) * zero(z) => xor(x,y,z) |
      |  zero(x) * one(y) * one(z) => xor(x,y,z) |
      |  one(x) * zero(y) * one(z) => xor(x,y,z) |
      |  one(x) * one(y) * zero(z) => xor(x,y,z)
      |} ;
      |
      |and {
      |  zero(x) * zero(z) => and(x,y,z) |
      |  zero(y) * zero(z) => and(x,y,z) |
      |  one(x) * one(y) * one(z) => and(x,y,z)
      |} ;
      |
      |one {
      |  x!=nil => one(x)
      |} ;
      |
      |zero {
      |  x=nil => zero(x)
      |}
    """.stripMargin

  def fullExample6 =
    """
      |p1 {
      |  x41=x10 * x9=x19 * x28!=nil * x43!=x37 * x17!=x2 * x40->nil * x27->x3 * x35->x22 * p2(x17,x37,x9) * p4(x6,x7,nil) => p1(x1,x2,x3) |
      |  x15=x13 * x33=x3 * x1=x39 * x41!=x3 * x40->nil * p2(x6,x21,x25) * p4(x23,x30,x38) => p1(x1,x2,x3) |
      |  x45=x26 * x34=x1 * x47=x19 * x47=x43 * x4!=nil * nil!=x1 * x45!=x17 * p1(x9,x9,x40) * p3(x32,x27,x43) => p1(x1,x2,x3) |
      |  x37=x1 * x3=x29 * x32=x26 * x2=x42 * x6!=x2 * x33!=x48 * x25!=x29 * x10->x32 * p3(x35,x18,x3) => p1(x1,x2,x3)
      |} ;
      |
      |p2 {
      |  x29=x49 * nil=x14 * x50!=x2 * x36!=x50 * x47!=x7 * x18->x42 * x1->x8 * x48->x6 * x34->x2 * p2(x9,x23,x21) * p1(x31,x36,x32) => p2(x1,x2,x3) |
      |  x12=x7 * x3!=x1 * x37!=x14 * x20->x14 * x2->x27 * x19->x32 * x11->x16 * x44->nil => p2(x1,x2,x3) |
      |  x3=x7 * x11=x8 * x24=x5 * x17=x25 * x2!=x28 * x5->x18 * x1->x22 * p1(x44,x5,x24) * p1(x23,x37,x24) * p3(x44,x3,x18) => p2(x1,x2,x3) |
      |  x7=x6 * x13=x1 * x23=x26 * x14=x45 * x1!=x45 * x26!=x29 * x17->x4 * x37->x21 * x35->x38 => p2(x1,x2,x3)
      |} ;
      |
      |p3 {
      |  x27=nil * x18=x14 * x49=x27 * x37!=x11 * x15!=x32 * nil!=x1 * x3->x8 * p1(x6,x4,x2) * p1(x3,x39,x4) * p1(x19,x34,x27) * p2(x2,x17,x3) * p1(x29,x6,x43) * p4(x15,x3,x1) => p3(x1,x2,x3) |
      |  x40=x37 * x2=x45 * x7=x27 * x7=x12 * x25->x1 * x3->x1 * p2(x20,x13,x2) * p2(x2,x18,x31) * p3(nil,x20,x27) => p3(x1,x2,x3) |
      |  x29=x2 * x6=x3 * x9=nil * x11!=x38 * x3!=x27 * x35->x7 * x2->nil * x18->x13 * p1(x25,x50,x6) * p4(x24,x14,x40) * p2(x45,x18,x3) * p2(x20,x1,x1) => p3(x1,x2,x3) |
      |  x18=x45 * x35=x3 * x48=x46 * x41=x2 * x14=x2 * x42=x40 * x2->x41 * x35->x1 * p2(x26,x17,x25) => p3(x1,x2,x3)
      |} ;
      |
      |p4 {
      |  x1=x27 * x3!=x39 * x10->x15 * x18->x1 * p2(x25,x21,x37) => p4(x1,x2,x3) |
      |  x3=x4 * x38=x46 * x28=x3 * x3=x46 * x1=x49 * x5!=x19 * x8!=x30 * x32!=x18 * x2->x34 * x12->x38 => p4(x1,x2,x3) |
      |  x23=x46 * x1=x3 * x22=x11 * x3!=x42 * x8->x21 * x1->x32 * x21->x3 => p4(x1,x2,x3) |
      |  x13=x34 * x1=x14 * x42!=x2 * x36->x42 * x37->x28 * p2(x3,x21,x36) * p4(nil,x46,x1) => p4(x1,x2,x3)
      |  }
    """.stripMargin

}
