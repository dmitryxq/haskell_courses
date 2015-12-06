probLanguageGivenMessage :: String
    -> String
    -> HashMap String Integer
    -> HashMap String (HashMap String Integer)
    -> Double
probLanguageGivenMessage language message languageFrequency wordFrequencyByLanguage =
    probLanguage language languageFrequency *
    product (L.map
            (\word ->
            probWordGivenLanguage word language wordFrequencyByLanguage)
            (words message))

probLanguage :: String
    -> HashMap String Integer
    -> Double
probLanguage language languageFrequency =
    countLanguage / countTweets
  where
    countTweets = fromIntegral . sum $ elems languageFrequency
    countLanguage = fromIntegral $
                    lookupDefault 0 language languageFrequency

probWordGivenLanguage :: String
    -> String
    ->HashMap String (HashMap String Integer)
    -> Double
probWordGivenLanguage word language wordFrequencyByLanguage =
    countWordInLanguage / countWordsUsedInLanguage
  where
    countWordInLanguage = fromIntegral .
                          lookupDefault 0 word $
                          wordFrequencyByLanguage ! language
    countWordsUsedInLanguage = fromIntegral . sum . elems $
                               wordFrequencyByLanguage ! 
                               language
languageClassifierGivenMessage ::
    String
    -> (HashMap String Integer)
    -> (HashMap String (HashMap String Integer))
    -> [(String, Double)]
languageClassifierGivenMessage
    message languageFrequency wordFrequencyByLanguage =
    L.map (\language->
          (language,
           probLanguageGivenMessage
              language message languageFrequency wordFrequencyByLanguage))
          (keys languageFrequency)
maxClassifier :: [(String, Double)] -> (String, Double)
maxClassifier = L.maximumBy (comparing snd)



