FUNCTION RRATE,C,PM
  RR = PM * 0.
  jj = where( PM-c[3] LE 0., COMPL=kk )

  IF max(jj) GE 0 then RR[JJ]=1.

  RR[KK] = 1 + C[0] * ( 1-exp( -c[1] * ( PM[kk] - c[3] ) ^ c[2] ) )

  RETURN,RR

END
