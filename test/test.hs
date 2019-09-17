{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty  
import Test.Tasty.HUnit

import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as U

import ConnectedComponentLabeling (Connectivity(..), PixelL, Image, asssignLabels, asssignLabels_HighestComponentValue)
import qualified ConnectedComponentLabeling_specialized as Cs
import MassivExtensions (iFoldlMutM)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] 


image_0 :: Image  
image_0 = 
    A.fromLists' Seq $   
        [ [] ]


toLists_image_0L_4 :: [[PixelL]]
toLists_image_0L_4 = 
    [ [] ]

    
toLists_image_0L_8 :: [[PixelL]]
toLists_image_0L_8 = 
    [ [] ]       


image_a :: Image  
image_a = 
    A.fromLists' Seq $   
        [ [0] ]


toLists_image_aL_4 :: [[PixelL]]
toLists_image_aL_4 = 
    [ [(0,0)] ]

    
toLists_image_aL_8 :: [[PixelL]]
toLists_image_aL_8 = 
    [ [(0,0)] ]


image_b :: Image  
image_b = 
    A.fromLists' Seq $   
        [ [30] ]


toLists_image_bL_4 :: [[PixelL]]
toLists_image_bL_4 = 
    [ [(30,1)] ]

    
toLists_image_bL_8 :: [[PixelL]]
toLists_image_bL_8 = 
    [ [(30,1)] ]


image_c :: Image  
image_c = 
    A.fromLists' Seq $   
        [ [30,50] ]


toLists_image_cL_4 :: [[PixelL]]
toLists_image_cL_4 = 
    [ [(30,1),(50,1)] ]

    
toLists_image_cL_8 :: [[PixelL]]
toLists_image_cL_8 = 
    [ [(30,1),(50,1)] ]


image_d :: Image  
image_d = 
    A.fromLists' Seq $   
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
image_e = 
    A.fromLists' Seq $   
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
image_f = 
    A.fromLists' Seq $   
        [ [2, 0]
        , [0, 3]
        , [0, 1]
        ]    
        

image_g :: Image  
image_g = 
    A.fromLists' Seq $   
        [ [0, 1, 0, 1, 0]
        , [0, 0, 0, 2, 0]
        , [2, 0, 0, 0, 0]
        , [0, 3, 0, 0, 2]
        , [0, 1, 0, 0, 0]
        ]   


image_A :: Image  
image_A = -- http://www.imageprocessingplace.com/downloads_V3/root_downloads/tutorials/contour_tracing_Abeer_George_Ghuneim/connect.html
    A.fromLists' Seq $   
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


image_B :: Image   
image_B = -- https://en.wikipedia.org/wiki/Connected-component_labeling
    A.fromLists' Seq $   
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


asssignLabelsCs :: Cs.Connectivity -> Cs.Image -> Cs.ImageL
asssignLabelsCs con arr = x where (_, x) = Cs.asssignLabels_HighestComponentValue con arr


unitTests = testGroup "Unit tests" $ 
    [  testGroup "MassivExtensions" $    
        [ testCase "iFoldlMutM" $ do 
            let
                f :: forall m . (Source U Ix2 Int, Mutable U Ix2 Int, PrimMonad m) => m [(Int, Ix2)]
                f = do
                    let 
                        arr :: Array U Ix2 Int
                        arr = A.fromLists' Seq $
                            [ [1,2,3]
                            , [4,5,6]
                            , [7,8,9]
                            ]
                    
                    marr <- thawS arr

                    let 
                        g :: Ix2 -> [(Int, Ix2)] -> m [(Int, Ix2)]
                        g ix acc = do
                            e <- unsafeRead marr ix
                            pure $ acc ++ [(e, ix)] 

                    iFoldlMutM g [] marr 

            result <- f 
            result @?= [(1,0 :. 0),(2,0 :. 1),(3,0 :. 2),(4,1 :. 0),(5,1 :. 1),(6,1 :. 2),(7,2 :. 0),(8,2 :. 1),(9,2 :. 2)]
        ]
        
    ,  testGroup "ConnectedComponentLabeling" $    
        [ testCase "0 asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabels Connect_4 image_0) @?= toLists_image_0L_4

        , testCase "0 asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabels Connect_8 image_0) @?= toLists_image_0L_8 

        , testCase "a asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabels Connect_4 image_a) @?= toLists_image_aL_4

        , testCase "a asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabels Connect_8 image_a) @?= toLists_image_aL_8 
            
        , testCase "b asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabels Connect_4 image_b) @?= toLists_image_bL_4

        , testCase "b asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabels Connect_8 image_b) @?= toLists_image_bL_8                           
            
        , testCase "c asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabels Connect_4 image_c) @?= toLists_image_cL_4

        , testCase "c asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabels Connect_8 image_c) @?= toLists_image_cL_8  
                        
        , testCase "d asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabels Connect_4 image_d) @?= toLists_image_dL_4

        , testCase "d asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabels Connect_8 image_d) @?= toLists_image_dL_8                         
                        
        , testCase "e asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabels Connect_4 image_e) @?= toLists_image_eL_4

        , testCase "e asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabels Connect_8 image_e) @?= toLists_image_eL_8  

        , testCase "A asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabels Connect_4 image_A) @?= toLists_image_AL_4

        , testCase "A asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabels Connect_8 image_A) @?= toLists_image_AL_8

        , testCase "B asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabels Connect_4 image_B) @?= toLists_image_BL_4

        , testCase "B asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabels Connect_8 image_B) @?= toLists_image_BL_8
        ]

    ,  testGroup "ConnectedComponentLabeling Cs" $    
        [ testCase "0 asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_4 image_0) @?= toLists_image_0L_4

        , testCase "0 asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_8 image_0) @?= toLists_image_0L_8 

        , testCase "a asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_4 image_a) @?= toLists_image_aL_4

        , testCase "a asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_8 image_a) @?= toLists_image_aL_8 
            
        , testCase "b asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_4 image_b) @?= toLists_image_bL_4

        , testCase "b asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_8 image_b) @?= toLists_image_bL_8                           
            
        , testCase "c asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_4 image_c) @?= toLists_image_cL_4

        , testCase "c asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_8 image_c) @?= toLists_image_cL_8  
                        
        , testCase "d asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_4 image_d) @?= toLists_image_dL_4

        , testCase "d asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_8 image_d) @?= toLists_image_dL_8                         
                        
        , testCase "e asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_4 image_e) @?= toLists_image_eL_4

        , testCase "e asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_8 image_e) @?= toLists_image_eL_8  

        , testCase "A asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_4 image_A) @?= toLists_image_AL_4

        , testCase "A asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_8 image_A) @?= toLists_image_AL_8

        , testCase "B asssignLabels Connect_4" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_4 image_B) @?= toLists_image_BL_4

        , testCase "B asssignLabels Connect_8" $ 
            (A.toLists $ asssignLabelsCs Cs.Connect_8 image_B) @?= toLists_image_BL_8
        ]

    ,  testGroup "HighestComponentValue" $
        [ testCase "f Connect_4" $ 
            let 
                (n, _) = asssignLabels_HighestComponentValue Connect_4 image_f
            in
                n @?= 4             

        , testCase "f Connect_8" $ 
            let
                (n, _) = asssignLabels_HighestComponentValue Connect_8 image_f   
            in
                n @?= 6 

        , testCase "g Connect_4" $ 
            let 
                (n, _) = asssignLabels_HighestComponentValue Connect_4 image_g
            in
                n @?= 4             

        , testCase "g Connect_8" $ 
            let
                (n, _) = asssignLabels_HighestComponentValue Connect_8 image_g   
            in
                n @?= 6          
                
        , testCase "B Connect_4" $ 
            let 
                (n, _) = asssignLabels_HighestComponentValue Connect_4 image_B
            in
                n @?= 25            

        , testCase "B Connect_8" $ 
            let
                (n, _) = asssignLabels_HighestComponentValue Connect_8 image_B   
            in
                n @?= 32                        
        ]     
        

    ,  testGroup "HighestComponentValue Cs" $
        [ testCase "f Connect_4" $ 
            let 
                (n, _) = Cs.asssignLabels_HighestComponentValue Cs.Connect_4 image_f
            in
                n @?= 4             

        , testCase "f Connect_8" $ 
            let
                (n, _) = Cs.asssignLabels_HighestComponentValue Cs.Connect_8 image_f   
            in
                n @?= 6 

        , testCase "g Connect_4" $ 
            let 
                (n, _) = Cs.asssignLabels_HighestComponentValue Cs.Connect_4 image_g
            in
                n @?= 4             

        , testCase "g Connect_8" $ 
            let
                (n, _) = Cs.asssignLabels_HighestComponentValue Cs.Connect_8 image_g   
            in
                n @?= 6          
                
        , testCase "B Connect_4" $ 
            let 
                (n, _) = Cs.asssignLabels_HighestComponentValue Cs.Connect_4 image_B
            in
                n @?= 25            

        , testCase "B Connect_8" $ 
            let
                (n, _) = Cs.asssignLabels_HighestComponentValue Cs.Connect_8 image_B   
            in
                n @?= 32                        
        ]         
    ]            