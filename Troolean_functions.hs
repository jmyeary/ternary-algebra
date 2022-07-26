--We begin by construction a data type called Troolean with three constructors False', True', Undetermined. False' and True' work as you'd expect, but Undetermined operates as a kind of extension of the Booleans. I use apostraphes in F and T so as not to confuse Haskell with the False and True data types in Prelude module. Next I have listed all 27 (3^3) unary functions. I am beginning now to use unary functions to construct dyadic trinary functions. I will continue to develop this as I learn more!

data Troolean = False' | Undetermined | True'
               deriving (Show, Enum, Read, Eq, Ord, Bounded)

ternToInt :: Troolean -> Int               --This function maps {F,U,T} onto {-1,0,1}
ternToInt True' = 1
ternToInt Undetermined = 0
ternToInt False' = (-1)

intToTern :: Int -> Troolean               --Inverse function of ternToInt
intToTern 1    = True'
intToTern 0    = Undetermined
intToTern (-1) = False'


--Constant monadic operators
mFalse :: Troolean -> Troolean              --Constant False
mFalse False'        = False'
mFalse Undetermined  = False'
mFalse True'         = False'

mTrue :: Troolean -> Troolean               --Constant True
mTrue False'         = True'
mTrue Undetermined   = True'
mTrue True'          = True'

mUndet :: Troolean -> Troolean               --Constant Undetermined
mUndet False'        = Undetermined
mUndet Undetermined  = Undetermined
mUndet True'         = Undetermined

buffer :: Troolean -> Troolean               --Unary identity function
buffer False'           = False'
buffer Undetermined     = Undetermined
buffer True'            = True'

--Decoders
decodeF:: Troolean -> Troolean              --Decode(-1): Inverts False' else returns False'.
decodeF False'       = True'
decodeF Undetermined = False'
decodeF True'        = False'

decodeU :: Troolean -> Troolean             --Decode 0: Returns True' on Undetermined, otherwise returns False'
decodeU False'       = False'
decodeU Undetermined = True'
decodeU True'        = False'

decodeT :: Troolean -> Troolean              --Decode1: Returns True' on True' else returns False'
decodeT False'       = False'
decodeT Undetermined = False'
decodeT True'        = True'

--Troolean Inverters
eNTI :: Troolean -> Troolean                --Earthed Negative Troolean Inverter. Maps False' to Undetermined else returns False'
eNTI False'          = Undetermined
eNTI Undetermined    = False'
eNTI True'           = False'

ePTI :: Troolean -> Troolean                --Earthed Positive Troolean Inverter. Maps True' to Undetermined else returns True'
ePTI False'          = True'
ePTI Undetermined    = True'
ePTI True'           = Undetermined

mPTI = decodeT                            --Positive Troolean Inverter. Alternate function name for Decode1.

mNTI = decodeF                            --Negative Troolean Inverter. Alternate function name for DecodeI where I = -1.

--Cyclic functions
cUp :: Troolean -> Troolean                 --Cycle up: Mimics addition by 1 modulo 3
cUp False'           = Undetermined
cUp Undetermined     = True'
cUp True'            = False'

cDown :: Troolean -> Troolean                --Cycle down: Mimics subtraction by 1 module 3
cDown False'         = True'
cDown Undetermined   = False'
cDown True'          = Undetermined

sTDown :: Troolean -> Troolean              --Shifts True down to Undetermined else returns False'
sTDown False'        = False'
sTDown Undetermined  = False'
sTDown True'         = Undetermined

--Swaps
swapUT :: Troolean -> Troolean               --Swaps Undetermined with True, fixes False
swapUT False'        = False'
swapUT Undetermined  = True'
swapUT True'         = Undetermined
swapTU = swapUT

swapFU :: Troolean -> Troolean               --Swaps Undetermined with False, fixes True
swapFU False'        = Undetermined
swapFU Undetermined  = False'
swapFU True'         = True'
swapUF = swapFU

swapFT :: Troolean -> Troolean             --Symmetric Gate: Fixes Undetermined, inverts True' and False'
swapFT False'          = True'
swapFT Undetermined    = Undetermined
swapFT True'           = False'
swapTF = swapFT

--Diodes. DW Jones calls them "Clamps"
rDiode :: Troolean -> Troolean               --Reverse diode. Fixes False', else returns Undetermined.
rDiode False'        = False'
rDiode Undetermined  = Undetermined
rDiode True'         = Undetermined

fDiode :: Troolean -> Troolean               --Forward Diode. Fixes True', else returns Undetermined.
fDiode False'        = Undetermined
fDiode Undetermined  = Undetermined
fDiode True'         = True'

--Inverse Decoders
decodeF_inv :: Troolean -> Troolean                --Inverse of DecodeI where I = -1
decodeF_inv False'            = False'
decodeF_inv Undetermined      = True'
decodeF_inv True'             = True'

decodeU_inv :: Troolean -> Troolean                --Inverse of Decode0
decodeU_inv False'            = True'
decodeU_inv Undetermined      = False'
decodeU_inv True'             = True'

decodeT_inv :: Troolean -> Troolean                --Inverse of Decode1
decodeT_inv False'            = True'
decodeT_inv Undetermined      = True'
decodeT_inv True'             = False'

--These functions didn't fit in anywhere else
mA :: Troolean -> Troolean
mA False'            = Undetermined
mA Undetermined      = False'
mA True'             = Undetermined

mT :: Troolean -> Troolean
mT False'            = True'
mT Undetermined      = Undetermined
mT True'             = True'

mX :: Troolean -> Troolean
mX False'            = Undetermined
mX Undetermined      = True'
mX True'             = True'

mG :: Troolean -> Troolean
mG False'            = Undetermined
mG Undetermined      = True'
mG True'             = Undetermined

mE :: Troolean -> Troolean
mE False'            = True'
mE Undetermined      = Undetermined
mE True'             = Undetermined

m3 :: Troolean -> Troolean
m3 False'            = False'
m3 Undetermined      = Undetermined
m3 True'             = False'

m4 :: Troolean -> Troolean
m4 False'            = Undetermined
m4 Undetermined      = Undetermined
m4 True'             = False'

--Now we will begin to implement dyadic ternary functions. In other words functions that take two ternary values and output a ternary value. In ternary logic there are 19,683 different possible ternary functions. However, as pointed out by Connelly in his paper 3-Trit Computer Architecture, we only care about Abelian functions. IE for some function f and troolean expressions A, B, we only care about those functions such that f A B = f B A. With this constraint implemented, we reduce the number of possibilities to 729: a reduction in size of over 96%! All functions are curryable, so we can write dyadic ternary functions in terms of partial compositions of functions. f(A,B)=f(False',B) ++ f(True',B) ++ f(Undetermined,B)

--Trinary And/Minimum = FFF,FUU,FUT
tAnd :: Troolean -> Troolean -> Troolean
tAnd False' b  = False'
tAnd Undetermined b  = rDiode b
tAnd True' b  = buffer b

--Trinary Or/Maximum = FUT, UUT, TTT


--Trinary NAnd = TTT, TUU, TUF


--Trinary NOr =TUF, UUF, FFF


-- SOURCES
-- Dr H.T. Mouftah, Department of Electrical Engineering, U of Toronto. "A Study on the Implementation of Three-Valued Logic"
-- Jeff Connelly, Computer Science and Engineering Dept, California Polytechnic State University, "Ternary Computing Testbed 3-Trit Computer Architecture"
-- Dr Douglas W Jones, Computer Science Department, University of Iowa "Standard Ternary Logic"
