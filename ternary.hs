--import Data.List (nub, delete)
--We begin by construction a data type called Ternary with three constructors False', True', Undetermined. False' and True' work as you'd expect, but Undetermined operates as a boolean value. Next I have listed all 27 possible monadic function (operate on one datatype

data Ternary = False' | Undetermined | True'

m0 :: Ternary -> Ternary
m0 True' = False'
m0 Undetermined = False'
m0 False' = False'

m1 :: Ternary -> Ternary
m1 True' = False'
m1 Undetermined = False'
m1 False' = Undetermined

m2 :: Ternary -> Ternary
m2 True' = False'
m2 Undetermined = False'
m2 False' = True'

m3 :: Ternary -> Ternary
m3 True' = False'
m3 Undetermined = Undetermined
m3 False' = False'

m4 :: Ternary -> Ternary
m4 True' = False'
m4 Undetermined = Undetermined
m4 False' = Undetermined

m5 :: Ternary -> Ternary
m5 True' = False'
m5 Undetermined = Undetermined
m5 False' = True'

m6 :: Ternary -> Ternary
m6 True' = False'
m6 Undetermined = True'
m6 False' = False'

m7 :: Ternary -> Ternary
m7 True' = False'
m7 Undetermined = True'
m7 False' = Undetermined

m8 :: Ternary -> Ternary
m8 True' = False'
m8 Undetermined = True'
m8 False' = True'

m9 :: Ternary -> Ternary
m9 True' = Undetermined
m9 Undetermined = False'
m9 False' = False'

mA :: Ternary -> Ternary
mA True' = Undetermined
mA Undetermined = False'
mA False' = Undetermined

mB :: Ternary -> Ternary
mB True' = Undetermined
mB Undetermined = False'
mB False' = True'

mC :: Ternary -> Ternary
mC True' = Undetermined
mC Undetermined = Undetermined
mC False' = False'

mD :: Ternary -> Ternary
mD True' = Undetermined
mD Undetermined = Undetermined
mD False' = Undetermined

mE :: Ternary -> Ternary
mE True' = Undetermined
mE Undetermined = Undetermined
mE False' = True'

mF :: Ternary -> Ternary
mF True' = Undetermined
mF Undetermined = True'
mF False' = False'

mG :: Ternary -> Ternary
mG True' = Undetermined
mG Undetermined = True'
mG False' = Undetermined

mH :: Ternary -> Ternary
mH True' = Undetermined
mH Undetermined = True'
mH False' = True'

mK :: Ternary -> Ternary
mK True' = True'
mK Undetermined = False'
mK False' = False'

mM :: Ternary -> Ternary
mM True' = True'
mM Undetermined = False'
mM False' = Undetermined

mN :: Ternary -> Ternary
mN True' = True'
mN Undetermined = False'
mN False' = True'

mP :: Ternary -> Ternary
mP True' = True'
mP Undetermined = Undetermined
mP False' = False'

mR :: Ternary -> Ternary
mR True' = True'
mR Undetermined = Undetermined
mR False' = Undetermined

mT :: Ternary -> Ternary
mT True' = True'
mT Undetermined = Undetermined
mT False' = True'

mV :: Ternary -> Ternary
mV True' = True'
mV Undetermined = True'
mV False' = False'

mX :: Ternary -> Ternary
mX True' = True'
mX Undetermined = True'
mX False' = Undetermined

mZ :: Ternary -> Ternary
mZ True' = True'
mZ Undetermined = True'
mZ False' = True'

monadic_functions = [m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,mA,mB,mC,mD,mE,mF,mG,mH,mK,mM,mN,mP,mR,mT,mV,mX,mZ]
monadic_const     = [mFalse=m0, mUndet=mD, mTrue=mZ, mId=mP]
monadic_helpers   = [mNegate=m5, mIncrement=m7, mDecrement=mB, mDecodeF=m2, mDecodeU=m6, mDecodeT=mK]
ternary_values = [False', Undetermined, True']



-- SOURCES
-- Dr H.T. Mouftah, Department of Electrical Engineering, U of Toronto. "A Study on the Implementation of Three-Valued Logic"
-- Jeff Connelly, Computer Science and Engineering Dept, California Polytechnic State University, "Ternary Computing Testbed 3-Trit Computer Architecture"
-- Dr Douglas W Jones, Computer Science Department, University of Iowa "Standard Ternary Logic"
