
calculateStep :: Int -> Int
calculateStep 0 = 0
calculateStep 1 = 1
calculateStep 3000 = 15
calculateStep x = round $ 1.8688 * (1.6357 ^ x)


closestStep :: Int -> Int
closestStep 0 = 0
closestStep 1 = 1
closestStep 3000 = 15
closestStep y = round $ logBase 1.6357 ((fromIntegral y) / 1.8688)

