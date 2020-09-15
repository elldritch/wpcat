module Main (main) where

import Relude
import Relude.Unsafe (fromJust)
import Text.HTML.Scalpel (attr, texts, (//), text, hasClass, (@:), chroots, Scraper, scrapeURL)
import Control.Monad.Extra (sequenceUntil)
import Data.Time (zonedTimeToUTC, UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Text (dropAround)

main :: IO ()
main = do
  pages <- concat <$> sequenceUntil null (map scrapePage [1 .. ])
  forM_ pages print
  where
    scrapePage :: Int -> IO [Post]
    scrapePage n = fromJust <$> scrapeURL ("https://acoup.blog/page/" <> show n <> "/") postScraper

data Post = Post
  { title :: Text,
    tags :: [Text],
    published :: UTCTime
  }
  deriving (Show)

postScraper :: Scraper Text [Post]
postScraper =
  chroots ("article" @: [hasClass "type-post"]) $ do
    title <- text $ "h1" @: [hasClass "entry-title"]
    tags <- texts $ "span" @: [hasClass "cat-links"] // "a"
    timestamp <- attr "datetime" "time"
    published <- zonedTimeToUTC <$> iso8601ParseM (toString $ dropAround (== '"') timestamp)
    return Post { title, tags, published }
