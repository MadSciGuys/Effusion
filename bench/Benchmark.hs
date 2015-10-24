import Criterion.Main
import Effusion

main = defaultMain
    [ bgroup "ngrams" 
        [ bench "10" $ whnf ngrams [1..10]
        , bench "100" $ whnf ngrams [1..100]
        , bench "1000" $ whnf ngrams [1..1000]
        ]
    ]
