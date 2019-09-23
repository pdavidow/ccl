{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

--- for QuickCheck -------------------
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
--------------------------------------

import Test.Tasty  
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as U
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map

import qualified CCL
import qualified CCL'
import CCL_def (Connectivity(..), PixelL, Image, ImageL)
import CCL_util (componentCount, componentSizes, componentValues, highestComponentValue, sumComponentSizes, sumComponentValues)
import MassivExtensions (iFoldlMutM)


main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [properties, unitTests] 


maxCount_Connect_4 :: Sz Ix2 -> Int
maxCount_Connect_4 (Sz (Ix2 i j)) = -- checker-board pattern
    let
        totalCount = i * j
        half = totalCount `div` 2            
    in
        if even totalCount then half else half + 1
    

maxCount_Connect_8 :: Sz Ix2 -> Int
maxCount_Connect_8 (Sz (Ix2 i j)) = -- packed non-touching
    let
        width = if even i then i else i + 1
        maxPerRow = width `div` 2

        height = if even j then j else j + 1
        rows = height `div` 2
    in
        maxPerRow * rows  


image_0 :: Image  
image_0 = A.fromLists' Seq $   
    [ [] ]


toLists_image_0L :: [[PixelL]]
toLists_image_0L = 
    [ [] ]  


image_a :: Image  
image_a = A.fromLists' Seq $   
    [ [0] ]


toLists_image_aL :: [[PixelL]]
toLists_image_aL = 
    [ [(0,0)] ]


image_b :: Image  
image_b = A.fromLists' Seq $   
    [ [30] ]


toLists_image_bL_4 :: [[PixelL]]
toLists_image_bL_4 = 
    [ [(30,1)] ]

    
toLists_image_bL_8 :: [[PixelL]]
toLists_image_bL_8 = 
    [ [(30,1)] ]


image_c :: Image  
image_c = A.fromLists' Seq $   
    [ [30,50] ]


toLists_image_cL_4 :: [[PixelL]]
toLists_image_cL_4 = 
    [ [(30,1),(50,1)] ]

    
toLists_image_cL_8 :: [[PixelL]]
toLists_image_cL_8 = 
    [ [(30,1),(50,1)] ]


image_d :: Image  
image_d = A.fromLists' Seq $   
    [ [30, 0]
    , [ 0,50]
    ]


toLists_image_dL_4 :: [[PixelL]]
toLists_image_dL_4 = 
    [ [(30,1),( 0,0)]
    , [( 0,0),(50,2)]
    ]


toLists_image_dL_8 :: [[PixelL]]
toLists_image_dL_8 = 
    [ [(30,1),( 0,0)]
    , [( 0,0),(50,1)]
    ]


image_e :: Image  
image_e = A.fromLists' Seq $   
    [ [30, 0, 0]
    , [ 0,50, 0]
    , [20,40,70]
    ]


toLists_image_eL_4 :: [[PixelL]]
toLists_image_eL_4 = 
    [ [(30,1),( 0,0),( 0,0)]
    , [( 0,0),(50,2),( 0,0)]
    , [(20,2),(40,2),(70,2)]
    ]


toLists_image_eL_8 :: [[PixelL]]
toLists_image_eL_8 = 
    [ [(30,1),( 0,0),( 0,0)]
    , [( 0,0),(50,1),( 0,0)]
    , [(20,1),(40,1),(70,1)]
    ]


image_f :: Image  
image_f = A.fromLists' Seq $   
    [ [2, 0]
    , [0, 3]
    , [0, 1]
    ]    
        

image_g :: Image  
image_g = A.fromLists' Seq $   
    [ [0, 1, 0, 1, 0]
    , [0, 0, 0, 2, 0]
    , [2, 0, 0, 0, 0]
    , [0, 3, 0, 0, 2]
    , [0, 1, 0, 0, 0]
    ]   


-- http://www.imageprocessingplace.com/downloads_V3/root_downloads/tutorials/contour_tracing_Abeer_George_Ghuneim/connect.html
image_A :: Image  
image_A = A.fromLists' Seq $   
    [ [0,0,0,0,0,0,0]
    , [0,0,0,1,1,0,0]
    , [0,0,1,0,0,1,0]
    , [0,1,0,0,1,0,0]
    , [0,0,1,1,1,1,0]
    , [0,1,0,0,0,0,0]
    , [0,0,0,0,0,0,0]
    ]


toLists_image_AL_4 :: [[PixelL]]
toLists_image_AL_4 = 
    [ [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]
    , [(0,0),(0,0),(0,0),(1,1),(1,1),(0,0),(0,0)]
    , [(0,0),(0,0),(1,2),(0,0),(0,0),(1,3),(0,0)]
    , [(0,0),(1,4),(0,0),(0,0),(1,5),(0,0),(0,0)]
    , [(0,0),(0,0),(1,5),(1,5),(1,5),(1,5),(0,0)]
    , [(0,0),(1,6),(0,0),(0,0),(0,0),(0,0),(0,0)]
    , [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]
    ]


toLists_image_AL_8 :: [[PixelL]]
toLists_image_AL_8 = 
    [ [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]
    , [(0,0),(0,0),(0,0),(1,1),(1,1),(0,0),(0,0)]
    , [(0,0),(0,0),(1,1),(0,0),(0,0),(1,1),(0,0)]
    , [(0,0),(1,1),(0,0),(0,0),(1,1),(0,0),(0,0)]
    , [(0,0),(0,0),(1,1),(1,1),(1,1),(1,1),(0,0)]
    , [(0,0),(1,1),(0,0),(0,0),(0,0),(0,0),(0,0)]
    , [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]
    ]    


-- https://en.wikipedia.org/wiki/Connected-component_labeling
image_B :: Image   
image_B = A.fromLists' Seq $   
    [ [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    , [0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0]
    , [0,1,1,1,1,1,1,1,1,0,0,1,1,1,1,0,0]
    , [0,0,0,1,1,1,1,0,0,0,1,1,1,1,0,0,0]
    , [0,0,1,1,1,1,0,0,0,1,1,1,0,0,1,1,0]
    , [0,1,1,1,0,0,1,1,0,0,0,1,1,1,0,0,0]
    , [0,0,1,1,0,0,0,0,0,1,1,0,0,0,1,1,0]
    , [0,0,0,0,0,0,1,1,1,1,0,0,1,1,1,1,0]
    , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    ] 
        

toLists_image_BL_4 :: [[PixelL]]
toLists_image_BL_4 = 
    [ [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]
    , [(0,0),(0,0),(1,1),(1,1),(0,0),(0,0),(1,1),(1,1),(0,0),(0,0),(1,2),(1,2),(0,0),(0,0),(1,2),(1,2),(0,0)]
    , [(0,0),(1,1),(1,1),(1,1),(1,1),(1,1),(1,1),(1,1),(1,1),(0,0),(0,0),(1,2),(1,2),(1,2),(1,2),(0,0),(0,0)]
    , [(0,0),(0,0),(0,0),(1,1),(1,1),(1,1),(1,1),(0,0),(0,0),(0,0),(1,2),(1,2),(1,2),(1,2),(0,0),(0,0),(0,0)]
    , [(0,0),(0,0),(1,1),(1,1),(1,1),(1,1),(0,0),(0,0),(0,0),(1,2),(1,2),(1,2),(0,0),(0,0),(1,3),(1,3),(0,0)]
    , [(0,0),(1,1),(1,1),(1,1),(0,0),(0,0),(1,4),(1,4),(0,0),(0,0),(0,0),(1,2),(1,2),(1,2),(0,0),(0,0),(0,0)]
    , [(0,0),(0,0),(1,1),(1,1),(0,0),(0,0),(0,0),(0,0),(0,0),(1,5),(1,5),(0,0),(0,0),(0,0),(1,6),(1,6),(0,0)]
    , [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(1,5),(1,5),(1,5),(1,5),(0,0),(0,0),(1,6),(1,6),(1,6),(1,6),(0,0)]
    , [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]
    ]

toLists_image_BL_8 :: [[PixelL]]
toLists_image_BL_8 = 
    [ [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]
    , [(0,0),(0,0),(1,1),(1,1),(0,0),(0,0),(1,1),(1,1),(0,0),(0,0),(1,2),(1,2),(0,0),(0,0),(1,2),(1,2),(0,0)]
    , [(0,0),(1,1),(1,1),(1,1),(1,1),(1,1),(1,1),(1,1),(1,1),(0,0),(0,0),(1,2),(1,2),(1,2),(1,2),(0,0),(0,0)]
    , [(0,0),(0,0),(0,0),(1,1),(1,1),(1,1),(1,1),(0,0),(0,0),(0,0),(1,2),(1,2),(1,2),(1,2),(0,0),(0,0),(0,0)]
    , [(0,0),(0,0),(1,1),(1,1),(1,1),(1,1),(0,0),(0,0),(0,0),(1,2),(1,2),(1,2),(0,0),(0,0),(1,2),(1,2),(0,0)]
    , [(0,0),(1,1),(1,1),(1,1),(0,0),(0,0),(1,1),(1,1),(0,0),(0,0),(0,0),(1,2),(1,2),(1,2),(0,0),(0,0),(0,0)]
    , [(0,0),(0,0),(1,1),(1,1),(0,0),(0,0),(0,0),(0,0),(0,0),(1,2),(1,2),(0,0),(0,0),(0,0),(1,2),(1,2),(0,0)]
    , [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(1,2),(1,2),(1,2),(1,2),(0,0),(0,0),(1,2),(1,2),(1,2),(1,2),(0,0)]
    , [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]
    ]


unitTests :: TestTree
unitTests = testGroup "Unit tests" $
    [  testGroup "MassivExtensions" $    
        [ testCase "iFoldlMutM" $ do 
            let
                resultM :: forall m . (Source U Ix2 Int, Mutable U Ix2 Int, PrimMonad m) => m [(Int, Ix2)]
                resultM = do
                    let 
                        arr :: Array U Ix2 Int
                        arr = A.fromLists' Seq $
                            [ [1,2,3]
                            , [4,5,6]
                            , [7,8,9]
                            ]
                    
                    marr <- thawS arr

                    let 
                        f :: Ix2 -> [(Int, Ix2)] -> m [(Int, Ix2)]
                        f ix acc = do
                            e <- unsafeRead marr ix
                            pure $ acc ++ [(e, ix)] 

                    iFoldlMutM f [] marr 

            result <- resultM 
            result @?= [(1,0 :. 0),(2,0 :. 1),(3,0 :. 2),(4,1 :. 0),(5,1 :. 1),(6,1 :. 2),(7,2 :. 0),(8,2 :. 1),(9,2 :. 2)]
        ]
        
    ,  testGroup "CCL" $    
        [ testCase "0 toLabeled Connect_4" $ 
            (A.toLists $ CCL.toLabeled Connect_4 image_0) @?= toLists_image_0L

        , testCase "0 toLabeled Connect_8" $ 
            (A.toLists $ CCL.toLabeled Connect_8 image_0) @?= toLists_image_0L

        , testCase "a toLabeled Connect_4" $ 
            (A.toLists $ CCL.toLabeled Connect_4 image_a) @?= toLists_image_aL

        , testCase "a toLabeled Connect_8" $ 
            (A.toLists $ CCL.toLabeled Connect_8 image_a) @?= toLists_image_aL
            
        , testCase "b toLabeled Connect_4" $ 
            (A.toLists $ CCL.toLabeled Connect_4 image_b) @?= toLists_image_bL_4

        , testCase "b toLabeled Connect_8" $ 
            (A.toLists $ CCL.toLabeled Connect_8 image_b) @?= toLists_image_bL_8                           
            
        , testCase "c toLabeled Connect_4" $ 
            (A.toLists $ CCL.toLabeled Connect_4 image_c) @?= toLists_image_cL_4

        , testCase "c toLabeled Connect_8" $ 
            (A.toLists $ CCL.toLabeled Connect_8 image_c) @?= toLists_image_cL_8  
                        
        , testCase "d toLabeled Connect_4" $ 
            (A.toLists $ CCL.toLabeled Connect_4 image_d) @?= toLists_image_dL_4

        , testCase "d toLabeled Connect_8" $ 
            (A.toLists $ CCL.toLabeled Connect_8 image_d) @?= toLists_image_dL_8                         
                        
        , testCase "e toLabeled Connect_4" $ 
            (A.toLists $ CCL.toLabeled Connect_4 image_e) @?= toLists_image_eL_4

        , testCase "e toLabeled Connect_8" $ 
            (A.toLists $ CCL.toLabeled Connect_8 image_e) @?= toLists_image_eL_8  

        , testCase "A toLabeled Connect_4" $ 
            (A.toLists $ CCL.toLabeled Connect_4 image_A) @?= toLists_image_AL_4

        , testCase "A toLabeled Connect_8" $ 
            (A.toLists $ CCL.toLabeled Connect_8 image_A) @?= toLists_image_AL_8

        , testCase "B toLabeled Connect_4" $ 
            (A.toLists $ CCL.toLabeled Connect_4 image_B) @?= toLists_image_BL_4

        , testCase "B toLabeled Connect_8" $ 
            (A.toLists $ CCL.toLabeled Connect_8 image_B) @?= toLists_image_BL_8
        ]

    ,  testGroup "CCL' " $    
        [ testCase "0 toLabeled Connect_4" $ 
            (A.toLists $ CCL'.toLabeled Connect_4 image_0) @?= toLists_image_0L

        , testCase "0 toLabeled Connect_8" $ 
            (A.toLists $ CCL'.toLabeled Connect_8 image_0) @?= toLists_image_0L

        , testCase "a toLabeled Connect_4" $ 
            (A.toLists $ CCL'.toLabeled Connect_4 image_a) @?= toLists_image_aL

        , testCase "a toLabeled Connect_8" $ 
            (A.toLists $ CCL'.toLabeled Connect_8 image_a) @?= toLists_image_aL
            
        , testCase "b toLabeled Connect_4" $ 
            (A.toLists $ CCL'.toLabeled Connect_4 image_b) @?= toLists_image_bL_4

        , testCase "b toLabeled Connect_8" $ 
            (A.toLists $ CCL'.toLabeled Connect_8 image_b) @?= toLists_image_bL_8                           
            
        , testCase "c toLabeled Connect_4" $ 
            (A.toLists $ CCL'.toLabeled Connect_4 image_c) @?= toLists_image_cL_4

        , testCase "c toLabeled Connect_8" $ 
            (A.toLists $ CCL'.toLabeled Connect_8 image_c) @?= toLists_image_cL_8  
                        
        , testCase "d toLabeled Connect_4" $ 
            (A.toLists $ CCL'.toLabeled Connect_4 image_d) @?= toLists_image_dL_4

        , testCase "d toLabeled Connect_8" $ 
            (A.toLists $ CCL'.toLabeled Connect_8 image_d) @?= toLists_image_dL_8                         
                        
        , testCase "e toLabeled Connect_4" $ 
            (A.toLists $ CCL'.toLabeled Connect_4 image_e) @?= toLists_image_eL_4

        , testCase "e toLabeled Connect_8" $ 
            (A.toLists $ CCL'.toLabeled Connect_8 image_e) @?= toLists_image_eL_8  

        , testCase "A toLabeled Connect_4" $ 
            (A.toLists $ CCL'.toLabeled Connect_4 image_A) @?= toLists_image_AL_4

        , testCase "A asssignLabels Connect_8" $ 
            (A.toLists $ CCL'.toLabeled Connect_8 image_A) @?= toLists_image_AL_8

        , testCase "B toLabeled Connect_4" $ 
            (A.toLists $ CCL'.toLabeled Connect_4 image_B) @?= toLists_image_BL_4

        , testCase "B toLabeled Connect_8" $ 
            (A.toLists $ CCL'.toLabeled Connect_8 image_B) @?= toLists_image_BL_8
        ]

    ,  testGroup "highestComponentValue $ CCL.toLabeled" $
        [ testCase "f Connect_4" $ 
            (highestComponentValue $ CCL.toLabeled Connect_4 image_f) @?= 4             

        , testCase "f Connect_8" $ 
            (highestComponentValue $ CCL.toLabeled Connect_8 image_f) @?= 6

        , testCase "g Connect_4" $ 
            (highestComponentValue $ CCL.toLabeled Connect_4 image_g) @?= 4          

        , testCase "g Connect_8" $ 
            (highestComponentValue $ CCL.toLabeled Connect_8 image_g) @?= 6       
                
        , testCase "B Connect_4" $ 
            (highestComponentValue $ CCL.toLabeled Connect_4 image_B) @?= 25          

        , testCase "B Connect_8" $ 
            (highestComponentValue $ CCL.toLabeled Connect_8 image_B) @?= 32                                                 
        ]             

    ,  testGroup "CCL'.highestComponentValue" $
        [ testCase "f Connect_4" $ 
            CCL'.highestComponentValue Connect_4 image_f @?= 4             

        , testCase "f Connect_8" $ 
            CCL'.highestComponentValue Connect_8 image_f @?= 6   

        , testCase "g Connect_4" $ 
            CCL'.highestComponentValue Connect_4 image_g @?= 4             

        , testCase "g Connect_8" $ 
            CCL'.highestComponentValue Connect_8 image_g @?= 6         
                
        , testCase "B Connect_4" $ 
            CCL'.highestComponentValue Connect_4 image_B @?= 25          

        , testCase "B Connect_8" $ 
            CCL'.highestComponentValue Connect_8 image_B @?= 32                       
        ]         

    ,  testGroup "CCL_util" $      
        [ testGroup "componentCount" $        
            [ testCase "0" $ 
                componentCount (A.fromLists' Seq toLists_image_BL_4) @?= 6

            , testCase "1" $ 
                componentCount (A.fromLists' Seq toLists_image_BL_8) @?= 2            
            ]

        , testGroup "componentSizes" $  
            [ testCase "0" $ 
                (Map.toAscList $ componentSizes (A.fromLists' Seq toLists_image_eL_4)) @?= [(1,1), (2,4)]
            ]    

        , testGroup "componentValues" $  
            [ testCase "0" $ 
                (Map.toAscList $ componentValues (A.fromLists' Seq toLists_image_eL_4)) @?= [(1,30), (2,180)]
            ]    

        , testGroup "maxCount_Connect_4" $  
            [ testCase "0 :. 0" $ 
                maxCount_Connect_4 (Sz (0 :. 0)) @?= 0

            , testCase "1 :. 1" $
                maxCount_Connect_4 (Sz (1 :. 1)) @?= 1    

            , testCase "2 :. 2" $ 
                maxCount_Connect_4 (Sz (2 :. 2)) @?= 2  

            , testCase "2 :. 3" $
                maxCount_Connect_4 (Sz (2 :. 3)) @?= 3                

            , testCase "3 :. 3" $  
                maxCount_Connect_4 (Sz (3 :. 3)) @?= 5    
                
            , testCase "4 :. 4" $  
                maxCount_Connect_4 (Sz (4 :. 4)) @?= 8  

            ,  testCase "5 :. 5" $ 
                maxCount_Connect_4 (Sz (5 :. 5)) @?= 13                           
            ]       

        , testGroup "maxCount_Connect_8" $  
            [ testCase "0 :. 0" $ 
                maxCount_Connect_8 (Sz (0 :. 0)) @?= 0

            , testCase "1 :. 1" $
                maxCount_Connect_8 (Sz (1 :. 1)) @?= 1    

            , testCase "2 :. 2" $ 
                maxCount_Connect_8 (Sz (2 :. 2)) @?= 1  

            , testCase "2 :. 3" $
                maxCount_Connect_8 (Sz (2 :. 3)) @?= 2

            , testCase "3 :. 3" $  
                maxCount_Connect_8 (Sz (3 :. 3)) @?= 4       
                
            , testCase "4 :. 4" $  
                maxCount_Connect_8 (Sz (4 :. 4)) @?= 4                    

            ,  testCase "5 :. 5" $ 
                maxCount_Connect_8 (Sz (5 :. 5)) @?= 9                           
            ]                 
        ]  
    ]            

-- ===================================================


properties :: TestTree
properties = testGroup "Properties" [qcProps]


-- stack repl
-- > :load test\test.hs
-- > generate arbitrary :: IO Image

instance Arbitrary Image where
    arbitrary :: Gen Image
    arbitrary = do
        numCols <- choose (1, 1000)
        numRows <- choose (1, 1000)

        e1 <- choose (1, 1000)
        e2 <- choose (1, 1000)
        e3 <- choose (1, 1000)

        let g1 = choose (0, 1)
        let g2 = choose (0, 2)
        let g3 = choose (0, 9)
        let g4 = elements [0, e1]
        let g5 = elements [0, 0, e2]
        let g6 = elements [0, e1, e2, e3]
        let g7 = elements [0, 0, 0, e1, e2, e3]
        let g8 = frequency [(99, pure 0), (1, pure 1)]
        let g9 = frequency [(1, pure 0), (99, pure 1)]        
        let g10 = frequency [(99, pure 0), (1, choose (1, 1000000000))]
        let g11 = frequency [(1, pure 0), (99, choose (1, 1000000000))]
        let gs = [g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11]

        xs <- V.replicateM numRows $ vectorOf numCols $ oneof gs
        let xss = V.map (\i -> xs V.! (i - 1)) $ V.fromList [1 .. numRows]
        pure $ A.fromLists' Seq $ V.toList xss


qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"    
    [ QC.testProperty "Connect_4 | CCL.toLabeled == CCL'.toLabeled" $
        \x -> CCL.toLabeled Connect_4 (x :: Image) == CCL'.toLabeled Connect_4 (x :: Image)

    , QC.testProperty "Connect_4 | highestComponentValue CCL.toLabeled == CCL'.highestComponentValue" $
        \x -> highestComponentValue (CCL.toLabeled Connect_4 (x :: Image)) == CCL'.highestComponentValue Connect_4 (x :: Image)

    , QC.testProperty "Connect_8 | CCL.toLabeled == CCL'.toLabeled" $
        \x -> CCL.toLabeled Connect_8 (x :: Image) == CCL'.toLabeled Connect_8 (x :: Image)

    , QC.testProperty "Connect_8 | highestComponentValue CCL.toLabeled == CCL'.highestComponentValue" $
        \x -> highestComponentValue (CCL.toLabeled Connect_8 (x :: Image)) == CCL'.highestComponentValue Connect_8 (x :: Image)

    , QC.testProperty "Connect_4 CCL | max componentCount" $ 
        \x -> componentCount (CCL.toLabeled Connect_4 (x :: Image)) <= maxCount_Connect_4 (size (x :: Image))

    , QC.testProperty "Connect_4 CCL' | max componentCount" $ 
        \x -> componentCount (CCL'.toLabeled Connect_4 (x :: Image)) <= maxCount_Connect_4 (size (x :: Image))  
        
    , QC.testProperty "Connect_8 CCL | max componentCount" $ 
        \x -> componentCount (CCL.toLabeled Connect_8 (x :: Image)) <= maxCount_Connect_8 (size (x :: Image))

    , QC.testProperty "Connect_8 CCL' | max componentCount" $ 
        \x -> componentCount (CCL'.toLabeled Connect_8 (x :: Image)) <= maxCount_Connect_8 (size (x :: Image))             
    
    , QC.testProperty "CCL | Connect_4 componentCount >= Connect_8 componentCount" $ 
        \x -> componentCount (CCL.toLabeled Connect_4 (x :: Image)) >= componentCount (CCL.toLabeled Connect_8 (x :: Image))
    
    , QC.testProperty "CCL' | Connect_4 componentCount >= Connect_8 componentCount" $ 
        \x -> componentCount (CCL'.toLabeled Connect_4 (x :: Image)) >= componentCount (CCL'.toLabeled Connect_8 (x :: Image))

    , QC.testProperty "CCL Connect_4 | sumComponentSizes <= elemsCount " $ 
        \x -> sumComponentSizes (CCL.toLabeled Connect_4 (x :: Image)) <= elemsCount (x :: Image)
       
    , QC.testProperty "CCL' Connect_4 | sumComponentSizes <= elemsCount " $ 
        \x -> sumComponentSizes (CCL'.toLabeled Connect_4 (x :: Image)) <= elemsCount (x :: Image)     
        
    , QC.testProperty "CCL Connect_8 | sumComponentSizes <= elemsCount " $ 
        \x -> sumComponentSizes (CCL.toLabeled Connect_8 (x :: Image)) <= elemsCount (x :: Image)
       
    , QC.testProperty "CCL' Connect_8 | sumComponentSizes <= elemsCount " $ 
        \x -> sumComponentSizes (CCL'.toLabeled Connect_8 (x :: Image)) <= elemsCount (x :: Image)       
               
    , QC.testProperty "CCL | Connect_4 sumComponentSizes == Connect_8 sumComponentSizes " $ 
        \x -> sumComponentSizes (CCL.toLabeled Connect_4 (x :: Image)) == sumComponentSizes (CCL.toLabeled Connect_8 (x :: Image))              
               
    , QC.testProperty "CCL' | Connect_4 sumComponentSizes == Connect_8 sumComponentSizes " $ 
        \x -> sumComponentSizes (CCL'.toLabeled Connect_4 (x :: Image)) == sumComponentSizes (CCL'.toLabeled Connect_8 (x :: Image))          

    , QC.testProperty "CCL | Connect_4 sumComponentValues == Connect_8 sumComponentValues " $ 
        \x -> sumComponentValues (CCL.toLabeled Connect_4 (x :: Image)) == sumComponentValues (CCL.toLabeled Connect_8 (x :: Image))              
               
    , QC.testProperty "CCL' | Connect_4 sumComponentValues == Connect_8 sumComponentValues " $ 
        \x -> sumComponentValues (CCL'.toLabeled Connect_4 (x :: Image)) == sumComponentValues (CCL'.toLabeled Connect_8 (x :: Image))          
    ]