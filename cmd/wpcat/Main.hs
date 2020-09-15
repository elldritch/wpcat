module Main (main) where

import Control.Monad.Extra (sequenceUntil)
import Data.Map (assocs, unionsWith)
import Data.Text (dropAround)
import Data.Time (UTCTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Relude
import Relude.Unsafe (fromJust)
import Text.HTML.Scalpel (Scraper, attr, chroots, hasClass, scrapeURL, text, texts, (//), (@:))

main :: IO ()
main = do
  posts <- concat <$> sequenceUntil null (map scrapePage [1 ..])
  let postsByTag = unionsWith (<>) $ concatMap (\p@Post {tags} -> map (\t -> one (t, [p])) tags) posts

  forM_ (assocs postsByTag) $ \(tag, postsWithTag) -> do
    putStrLn $ "# " <> toString tag
    forM_ (sortOn published postsWithTag) $ \Post{title} -> do
      putStrLn $ toString title
    putStrLn ""
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
    return Post {title, tags, published}
