data People = Yamada | Tanaka | Satou
instance Eq People where
    Yamada == Yamada = True
    Tanaka == Tanaka = True
    Satou == Satou = True
    _ == _ = False
instance Show People where
    show Yamada = "山田"
    show Tanaka = "田中"
    show Satou = "佐藤"

main = do
    print $ Yamada == Yamada
    print $ Yamada == Tanaka
    print $ Yamada

