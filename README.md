# Programa Vigas Contínuas

## Este programa foi desenvolvido em linguagem Visual Basic, e o desafio é transformá-lo em um aplicativo Flutter Multiplataforma.
O programa permite a inclusão de cargas concentradas e distribuídas nas vigas contínuas.

As cargas distribuídas podem ser uniformes ou trapezoidais.

No decorrer do desenvolvimento serão esclarecidos os conceitos e apresentadas as demonstrações de cálculo.

~~~~VBScript
inicio:
Clear
Color 11, 0
width "LPT1:",2000
Cls
Print Tab(17); "CALCULO DE VIGAS CONTINUAS - METODO DE CROSS"
Print
input"No. de Vaos";nv
n = 2 * nv
Dim F(n), M(n), L(nv), I(nv), K(nv), S(n), R(n), ma(nv, 50), q(nv, 50), p(nv, 50), mf(nv, 500), v(nv, 500)
For I = 1 To nv
    Print
    z = I: GoSub format
    Print "Vao : "; v$
    Print
    input"Comprimento : ";l(i)
    input"Inercia     : ";i(i)
    K(I) = 4 * I(I) / L(I)
    Print
    input"No. de cargas distribuidas ";nd
    If nd <= 0 Then GoTo conc
    q(I, 0) = nd
    For j = 1 To nd
        Print
        input"   Valor de <W> ";w
        input"   Valor de <Q> ";q
        input"   Valor de <A> ";a
        input"   Valor de <B> ";b
        c = L(I) - a - b
        If q > w Then
                   v1 = (q - w)
                   X1 = 2 / 3 * b + a
                   v2 = w
                 Else
                   v1 = (w - q)
                   X1 = 1 / 3 * b + a
                   v2 = q
         End If
         M(2 * I - 1) = M(2 * I - 1) + (v2 * (6 * L(I) ^ 2 * ((a + b) ^ 2 - a ^ 2) - 8 * L(I) * ((a + b) ^ 3 - a ^ 3) + 3 * ((a + b) ^ 4 - a ^ 4)) / 12 + v1 * b * (20 * b * c * (a + c) + 5 * b ^ 2 + (a + 2 * c) + 30 * a * c ^ 2 + 2 * b ^ 3) / 60) / L(I) ^ 2
         M(2 * I) = M(2 * I) + (-v2 * (4 * L(I) * ((a + b) ^ 3 - a ^ 3) - 3 * ((a + b) ^ 4 - a ^ 4)) / 12 - v1 * (10 * a * b * (a + b) + 15 * c * (2 * a ^ 2 + b ^ 2) + 40 * a * b * c + 3 * b ^ 3) / 60) / L(I) ^ 2
         q(I, 4 * j - 3) = w: q(I, 4 * j - 2) = q: q(I, 4 * j - 1) = a: q(I, 4 * j) = b
         If I = 1 Then m1 = m1 - v1 * b / 2 * (L(I) - X1) - v2 * b * (c + b / 2)
         If I = nv Then mn = mn - v1 * b / 2 * X1 + v2 * b * (a + b / 2)
Next j
conc:
Print
input"No. de cargas concentradas ";nc
If nc <= 0 Then GoTo mom
p(I, 0) = nc
For j = 1 To nc
    Print
    input"Valor de <P>";q
    input"Valor de <X>";a
    b = L(I) - a
    M(2 * I - 1) = M(2 * I - 1) + q * a * b ^ 2 / L(I) ^ 2
    M(2 * I) = M(2 * I) - q * a ^ 2 * b / L(I) ^ 2
    If I = 1 Then m1 = m1 - q * (L(I) - a)
    If I = nv Then mn = mn + q * a
    p(I, 2 * j - 1) = q: p(I, 2 * j) = a
Next j
mom:
Print
input "No. de momentos";nm
If nm <= 0 Then GoTo result
ma(I, 0) = nm
For j = 1 To nm
    Print
    input "Valor de <M>";m
    input "Valor de <X>";a
    b = L(I) - a
    M(2 * I - 1) = M(2 * I - 1) - M * b / L(I) * (2 - 3 * b / L(I))
    M(2 * I) = M(2 * I) - M * a / L(I) * (2 - 3 * a / L(I))
    If I = 1 Then m1 = m1 - M
    If I = nv Then mn = mn - M
    ma(I, 2 * j - 1) = M: ma(I, 2 * j) = a
Next j
result:
Print
Print "M.Engast. Esquerdo  : "; M(2 * I - 1)
Print
Print "M.Engast. Direito   : "; M(2 * I)
Next I
For I = 2 To n - 1 Step 2
    F(I) = K(I / 2) / (K(I / 2) + K(I / 2 + 1))
    F(I + 1) = K(I / 2 + 1) / (K(I / 2) + K(I / 2 + 1))
Next I
Print: Print "Identificacao dos Vinculos Extremos"
Print: Print "A => APOIO    E => ENGASTE    L => LIVRE"
Print
Identesq:
input "Vinculo extremo a esquerda ";ve$
If ve$ = "A" Or ve$ = "a" Then
                        F(1) = 1
                    ElseIf ve$ = "E" Or ve$ = "e" Then
                                                F(1) = 0
                                            ElseIf ve$ = "L" Or ve$ = "l" Then
                                                                        M(1) = 0
                                                                        M(2) = m1
                                                                        M(3) = M(3) - m1
                                                                        F(1) = 0
                                                                        F(2) = 0
                                                                        F(3) = 1
                                                                     Else
                                                                        GoTo Identesq

End If
identdir:
Print
input "Vinculo extremo a direita ";vd$
If vd$ = "A" Or vd$ = "a" Then
                        F(n) = 1
                    ElseIf vd$ = "E" Or vd$ = "e" Then
                                                F(n) = 0
                                            ElseIf vd$ = "L" Or vd$ = "l" Then
                                                                        M(n) = 0
                                                                        M(n - 1) = mn
                                                                        M(n - 2) = M(n - 2) - mn
                                                                        F(n) = 0
                                                                        F(n - 1) = 0
                                                                        F(n - 2) = 1
                                                                     Else
                                                                        GoTo identdir

End If
Print
Print "MOMENTOS DE ENGASTAMENTO E FATORES DE DISTRIBUICAO"
Print
For I = 1 To n
    Print using; "###      #,###,###,###.####      #,###,###,###.####"; I, M(I), F(I)
    S(I) = M(I)
Next I
Print
input"Quantos ciclos";l
z = L
ciclo:
M(1) = -M(1) * F(1): M(n) = -M(n) * F(n)
S(1) = S(1) + M(1): S(n) = S(n) + M(n)
For I = 2 To n - 1 Step 2
    K = M(I) + M(I + 1)
    M(I) = -K * F(I)
    M(I + 1) = -K * F(I + 1)
    S(I) = S(I) + M(I)
    S(I + 1) = S(I + 1) + M(I + 1)
Next I
L = L - 1
If L = 0 Then GoTo resultado
For I = 1 To n - 1 Step 2
    a = M(I)
    M(I) = M(I + 1) / 2
    S(I) = S(I) + M(I)
    M(I + 1) = a / 2
    S(I + 1) = S(I + 1) + M(I + 1)
Next I
GoTo ciclo
resultado:
lprint chr$(27)chr$(72)
lprint
lprint"MOMENTOS RESULTANTES APOS "z" ITERACOES":lprint
For I = 1 To n
    lprint using"M( ### )  = ###,###,###,###.## ";i,s(i)
Next I
For I = 1 To nv
    If q(I, 0) <> 0 Then
                   For j = 1 To q(I, 0)
                          w = q(I, 4 * j - 3): q = q(I, 4 * j - 2): a = q(I, 4 * j - 1): b = q(I, 4 * j): c = L(I) - a - b
                          If w > q Then
                                   R(2 * I - 1) = R(2 * I - 1) - q * b * (c + b / 2) / L(I) - (w - q) * b / 2 * (c + 2 / 3 * b) / L(I)
                                   R(2 * I) = R(2 * I) - q * b * (a + b / 2) / L(I) - (w - q) * b / 2 * (a + b / 3) / L(I)
                                 Else
                                   R(2 * I - 1) = R(2 * I - 1) - w * b * (c + b / 2) / L(I) - (q - w) * b / 2 * (c + 2 / 3 * b) / L(I)
                                   R(2 * I) = R(2 * I) - w * b * (a + b / 2) / L(I) - (q - w) * b / 2 * (a + 2 / 3 * b) / L(I)
                          End If
                   Next j
    End If
    If p(I, 0) <> 0 Then
                   For j = 1 To p(I, 0)
                       q = p(I, 2 * j - 1): a = p(I, 2 * j)
                       R(2 * I - 1) = R(2 * I - 1) - q * (L(I) - a) / L(I)
                       R(2 * I) = R(2 * I) - q * a / L(I)
                   Next j
    End If
    If ma(I, 0) <> 0 Then
                   For j = 1 To ma(I, 0)
                       M = ma(I, 2 * j - 1): a = ma(I, 2 * j)
                       R(2 * I - 1) = R(2 * I - 1) - M / L(I)
                       R(2 * I) = R(2 * I) + M / L(I)
                   Next j
    End If
R(2 * I - 1) = R(2 * I - 1) - ((S(2 * I - 1) + S(2 * I))) / L(I)
R(2 * I) = R(2 * I) + (S(2 * I - 1) + S(2 * I)) / L(I)
If I = 1 And UCase$(ve$) = "L" Then
                             R(2) = R(2) + R(1)
                             R(1) = 0
End If
If I = nv And UCase$(vd$) = "L" Then
                             R(2 * I - 1) = R(2 * I - 1) + R(2 * I)
                             R(2 * I) = 0
End If
Next I
lprint
lprint "REACOES NOS VINCULOS ": lprint
lprint "R 01   =>   ";r(1)
For I = 2 To nv
    z = I: GoSub format
    lprint "R "v$;"   =>   ";r(2*i-2)+r(2*i-1)
Next I
z = nv + 1: GoSub format
lprint "R "v$"   =>   ";r(n)
M = S(1): v = 0
For I = 1 To nv
    L = 0
    If I = 1 Then R = R(1) Else R = R(2 * I - 2) + R(2 * I - 1)
    For x = 0 To L(I) Step 0.1
      GoSub momflet
    Next x
    M = mf(I, L): v = -v(I, L)
Next I
lprint
For I = 1 To nv
    lprint using "Vao ### ";i
    L = 0
    For x = 0 To L(I) Step 0.1
         GoSub impflet
    Next x
    lprint
Next I
lprint Chr$(12)
lprint tab(20)"DIAGRAMA DE MOMENTOS FLETORES"
lprint
lprint chr$(27)chr$(65)chr$(6)
For I = 1 To nv
    L = 0
    For x = 0 To L(I) - 0.1 Step 0.1
        GoSub pontof
    Next x
Next I
I = nv
x = L(I)
GoSub pontof
lprint Chr$(12)
lprint tab(20)"DIAGRAMA DE ESFORCOS CORTANTES"
lprint: lprint
For I = 1 To nv
    L = 0
    For x = 0 To L(1) - 0.1 Step 0.1
        GoSub pontov
    Next x
Next I
I = nv
x = L(I)
GoSub pontov
s$=input$(1)
GoTo inicio
pontof:
incr L
M = Int(mf(I, L) * 100 / 5)
If M > 780 Then M = 780
If M < -780 Then M = -780
If x = L(I) And I = nv Then L = 1
If M > 0 Then S = 781 Else S = 781 - M
j = Int(S / 256): S = S - j * 256
lprint chr$(27)"L"chr$(s)chr$(j);
If M > 0 Then
          if l=1 then lprint string$(780-m,1); else lprint string$(780-m,0);
          lprint string$(m,3);chr$(255);
          lprint
        Else
          If L = 1 Then
                   lprint string$(780,1);
                   if i<>1 then lprint chr$(255); else lprint chr$(1);
                 Else
                   lprint string$(780,0);chr$(255);
           End If
           lprint string$(abs(m),3);
           lprint
End If
Return
pontov:
incr L
M = Int(v(I, L) * 100 / 5)
If x = L(I) And I = nv Then L = 1
If M > 780 Then M = 780
If M < -780 Then M = -780
If M > 0 Then S = 781 Else S = 781 - M
j = Int(S / 256): S = S - j * 256
lprint chr$(27)"L"chr$(s)chr$(j);
If M > 0 Then
          if l=1 then lprint string$(780-m,1); else lprint string$(780-m,0);
          lprint string$(m,3);chr$(255);
          lprint
        Else
          If L = 1 Then
                   lprint string$(780,1);
                   if i<>1 then lprint chr$(255); else lprint chr$(1);
                 Else
                   lprint string$(780,0);chr$(255);
           End If
           lprint string$(abs(m),3);
           lprint
End If
Return
impflet:
incr L
    lprint using "###.##   ##########.####    ##########.####";x,mf(i,l),v(i,l)
Return
momflet:
incr L
        mf(I, L) = M + R * x + v * x: v(I, L) = -v - R
        If q(I, 0) <> 0 Then
                        For j = 1 To q(I, 0)
                            w = q(I, 4 * j - 3): q = q(I, 4 * j - 2): a = q(I, 4 * j - 1): b = q(I, 4 * j)
                            If x >= a And x <= a + b Then
                                                 mf(I, L) = mf(I, L) + q * (x - a) ^ 2 / 2 + (w - q) * (x - a) ^ 3 / (3 * b)
                                                 v(I, L) = v(I, L) - q * (x - a) - (w - q) * (x - a) ^ 2 / (2 * b)
                            End If
                            If x > a + b Then
                                        mf(I, L) = mf(I, L) + q * b * (x - a - b / 2) + (w - q) * b / 2 * (x - a - 2 / 3 * b)
                                        v(I, L) = v(I, L) - (w + q) * b / 2
                            End If
                        Next j
        End If
        If p(I, 0) <> 0 Then
                        For j = 1 To p(I, 0)
                            q = p(I, 2 * j - 1): a = p(I, 2 * j)
                            If x >= a Then
                                       mf(I, L) = mf(I, L) + q * (x - a)
                                       v(I, L) = v(I, L) - q
                            End If
                        Next j
         End If
         If ma(I, 0) <> 0 Then
                         For j = 1 To ma(I, 0)
                             M = ma(I, 2 * j - 1): a = ma(I, 2 * j)
                             If x >= a Then
                                   mf(I, L) = mf(I, L) + M
                             End If
                         Next j
          End If
Return
format:
v$ = Str$(z): v$ = "0" + Right$(v$, Len(v$) - 1): v$ = Right$(v$, 2): Return
End
~~~~
