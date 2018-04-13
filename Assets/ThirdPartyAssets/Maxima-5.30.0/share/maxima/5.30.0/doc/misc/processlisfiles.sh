#!/bin/sh
#processlisfiles.sh - # This runs the x.awk script on the
#list of lis files. 
#Copyright (C) 2002 Dan Stanger.  Copyright is assigned to the 
#FSF.

#This library is free software; you can redistribute it and/or
#modify it under the terms of the GNU Lesser General Public
#License as published by the Free Software Foundation; either
#version 2.1 of the License, or (at your option) any later version.

#This library is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#Lesser General Public License for more details.

#You should have received a copy of the GNU Lesser General Public
#License along with this library; if not, write to the Free Software
#Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


awk -f x.awk <lisdir/acall.lis
awk -f x.awk <lisdir/algfac.lis
awk -f x.awk <lisdir/algsys.lis
awk -f x.awk <lisdir/ar.lis
awk -f x.awk <lisdir/askp.lis
awk -f x.awk <lisdir/asum.lis
awk -f x.awk <lisdir/bessel.lis
awk -f x.awk <lisdir/buildq.lis
awk -f x.awk <lisdir/cl-info.lis
awk -f x.awk <lisdir/clmacs.lis
awk -f x.awk <lisdir/combin.lis
awk -f x.awk <lisdir/comm.lis
awk -f x.awk <lisdir/comm2.lis
awk -f x.awk <lisdir/compar.lis
awk -f x.awk <lisdir/commac.lis
awk -f x.awk <lisdir/compar.lis
awk -f x.awk <lisdir/compat.lis
awk -f x.awk <lisdir/cpoly.lis
awk -f x.awk <lisdir/csimp.lis
awk -f x.awk <lisdir/csimp2.lis
awk -f x.awk <lisdir/db.lis
awk -f x.awk <lisdir/defcal.lis
awk -f x.awk <lisdir/defint.lis
awk -f x.awk <lisdir/defopt.lis
awk -f x.awk <lisdir/desoln.lis
awk -f x.awk <lisdir/displa.lis
awk -f x.awk <lisdir/displm.lis
awk -f x.awk <lisdir/dskfn.lis
awk -f x.awk <lisdir/elim.lis
awk -f x.awk <lisdir/ellipt.lis
awk -f x.awk <lisdir/evalw.lis
awk -f x.awk <lisdir/ezgcd.lis
awk -f x.awk <lisdir/factor.lis
awk -f x.awk <lisdir/fcall.lis
awk -f x.awk <lisdir/float.lis
awk -f x.awk <lisdir/fortra.lis
awk -f x.awk <lisdir/generr.lis
awk -f x.awk <lisdir/grind.lis
awk -f x.awk <lisdir/hayat.lis
awk -f x.awk <lisdir/homog.lis
awk -f x.awk <lisdir/hyp.lis
awk -f x.awk <lisdir/hypgeo.lis
awk -f x.awk <lisdir/inmis.lis
awk -f x.awk <lisdir/intpol.lis
awk -f x.awk <lisdir/invert.lis
awk -f x.awk <lisdir/irinte.lis
awk -f x.awk <lisdir/laplac.lis
awk -f x.awk <lisdir/ldisp.lis
awk -f x.awk <lisdir/lesfac.lis
awk -f x.awk <lisdir/letmac.lis
awk -f x.awk <lisdir/limit.lis
awk -f x.awk <lisdir/linnew.lis
awk -f x.awk <lisdir/lmdcls.lis
awk -f x.awk <lisdir/logarc.lis
awk -f x.awk <lisdir/macdes.lis
awk -f x.awk <lisdir/macsys.lis
awk -f x.awk <lisdir/mactex.lis
awk -f x.awk <lisdir/marray.lis
awk -f x.awk <lisdir/mat.lis
awk -f x.awk <lisdir/matcom.lis
awk -f x.awk <lisdir/matrix.lis
awk -f x.awk <lisdir/matrun.lis
awk -f x.awk <lisdir/maxmac.lis
awk -f x.awk <lisdir/mdebug.lis
awk -f x.awk <lisdir/mdefun.lis
awk -f x.awk <lisdir/mdot.lis
awk -f x.awk <lisdir/merror.lis
awk -f x.awk <lisdir/mforma.lis
awk -f x.awk <lisdir/mformt.lis
awk -f x.awk <lisdir/mhayat.lis
awk -f x.awk <lisdir/misc.lis
awk -f x.awk <lisdir/mlisp.lis
awk -f x.awk <lisdir/mload.lis
awk -f x.awk <lisdir/mmacro.lis
awk -f x.awk <lisdir/mopers.lis
awk -f x.awk <lisdir/mormac.lis
awk -f x.awk <lisdir/mrgmac.lis
awk -f x.awk <lisdir/mstuff.lis
awk -f x.awk <lisdir/mtrace.lis
awk -f x.awk <lisdir/mutils.lis
awk -f x.awk <lisdir/nalgfa.lis
awk -f x.awk <lisdir/newdet.lis
awk -f x.awk <lisdir/newinv.lis
awk -f x.awk <lisdir/nforma.lis
awk -f x.awk <lisdir/nisimp.lis
awk -f x.awk <lisdir/nparse.lis
awk -f x.awk <lisdir/nrat4.lis
awk -f x.awk <lisdir/nregex.lis
awk -f x.awk <lisdir/numer.lis
awk -f x.awk <lisdir/numerm.lis
awk -f x.awk <lisdir/numth.lis
awk -f x.awk <lisdir/nusum.lis
awk -f x.awk <lisdir/ode2.lis
awk -f x.awk <lisdir/opers.lis
awk -f x.awk <lisdir/optim.lis
awk -f x.awk <lisdir/option.lis
awk -f x.awk <lisdir/outmis.lis
awk -f x.awk <lisdir/pade.lis
awk -f x.awk <lisdir/plot.lis
awk -f x.awk <lisdir/pois2.lis
awk -f x.awk <lisdir/pois3.lis
awk -f x.awk <lisdir/polyrz.lis
awk -f x.awk <lisdir/procs.lis
awk -f x.awk <lisdir/psolve.lis
awk -f x.awk <lisdir/rat3a.lis
awk -f x.awk <lisdir/rat3b.lis
awk -f x.awk <lisdir/rat3c.lis
awk -f x.awk <lisdir/rat3d.lis
awk -f x.awk <lisdir/rat3e.lis
awk -f x.awk <lisdir/ratmac.lis
awk -f x.awk <lisdir/ratout.lis
awk -f x.awk <lisdir/ratpoi.lis
awk -f x.awk <lisdir/residu.lis
awk -f x.awk <lisdir/result.lis
awk -f x.awk <lisdir/risch.lis
awk -f x.awk <lisdir/rombrg.lis
awk -f x.awk <lisdir/rpart.lis
awk -f x.awk <lisdir/runtim.lis
awk -f x.awk <lisdir/rzmac.lis
awk -f x.awk <lisdir/schatc.lis
awk -f x.awk <lisdir/scs.lis
awk -f x.awk <lisdir/series.lis
awk -f x.awk <lisdir/simp.lis
awk -f x.awk <lisdir/sin.lis
awk -f x.awk <lisdir/sinint.lis
awk -f x.awk <lisdir/sloop.lis
awk -f x.awk <lisdir/solve.lis
awk -f x.awk <lisdir/specfn.lis
awk -f x.awk <lisdir/spgcd.lis
awk -f x.awk <lisdir/sprdet.lis
awk -f x.awk <lisdir/strmac.lis
awk -f x.awk <lisdir/sublis.lis
awk -f x.awk <lisdir/sumcon.lis
awk -f x.awk <lisdir/suprv1.lis
awk -f x.awk <lisdir/tlimit.lis
awk -f x.awk <lisdir/todd-coxeter.lis
awk -f x.awk <lisdir/trans1.lis
awk -f x.awk <lisdir/trans2.lis
awk -f x.awk <lisdir/trans3.lis
awk -f x.awk <lisdir/trans4.lis
awk -f x.awk <lisdir/trans5.lis
awk -f x.awk <lisdir/transf.lis
awk -f x.awk <lisdir/transl.lis
awk -f x.awk <lisdir/transm.lis
awk -f x.awk <lisdir/transq.lis
awk -f x.awk <lisdir/transs.lis
awk -f x.awk <lisdir/trdata.lis
awk -f x.awk <lisdir/trgred.lis
awk -f x.awk <lisdir/trgsmp.lis
awk -f x.awk <lisdir/trigi.lis
awk -f x.awk <lisdir/trigo.lis
awk -f x.awk <lisdir/trmode.lis
awk -f x.awk <lisdir/troper.lis
awk -f x.awk <lisdir/trpred.lis
awk -f x.awk <lisdir/trprop.lis
awk -f x.awk <lisdir/trutil.lis
awk -f x.awk <lisdir/ufact.lis
awk -f x.awk <lisdir/utils.lis
awk -f x.awk <lisdir/zero.lis
