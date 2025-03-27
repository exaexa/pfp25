import Data.Semigroup ((<>))
import Options.Applicative

theOption :: Parser Bool
theOption =
  switch $ long "good" <> short 'g' <> help "Should it be really good?"

optInfo =
  info (helper <*> theOption) $
  progDesc "good shows current level of goodness" <>
  header "A program that shows if it's good." <>
  footer "See more information on www.good.program.hu"

main = do
  opt <- execParser optInfo
  putStrLn $
    if opt
      then "it's good"
      else "not good at all!"
