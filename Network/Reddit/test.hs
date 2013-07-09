{-# LANGUAGE OverloadedStrings #-}

module Network.Reddit.Test where

import Network.Reddit.Types
import qualified Data.ByteString.Lazy as L
import Data.Aeson(Value,Result(..),encode)

import Debug.Trace
import Data.Word

convert :: (Enum a, Enum b) => a -> b
convert = toEnum . fromEnum

traceValueWith f a = traceShow (f a) a
jsonStr = putStr . map (convert :: Word8 -> Char). L.unpack . encode . (\(Success a) -> a) :: Result Value -> IO ()

sample_listing = [Link {link_rname = "t3_1hvgh1", link_info = ContentInfo {author = "Hyooz", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 1, downs = 1, likes = Nothing}, clicked = False, domain = "imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 1, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hvgh1/some_pictures_of_preprogrammed_video_game_happy/", saved = False, score = 0, selftext = Nothing, thumbnail = "http://f.thumbs.redditmedia.com/XcfAKixIG7D-wc6S.jpg", url = "http://imgur.com/a/kJlyV#0"},Link {link_rname = "t3_1hve3o", link_info = ContentInfo {author = "yotsubakoiwai", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 1, downs = 0, likes = Nothing}, clicked = False, domain = "imgur.com", hidden = False, is_self = False, link_flair = Nothing, num_comments = 1, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hve3o/a_new_playground_in_a_backyard/", saved = False, score = 1, selftext = Nothing, thumbnail = "http://c.thumbs.redditmedia.com/MA5aML2833cfzDBD.jpg", url = "http://imgur.com/a/KMYep"},Link {link_rname = "t3_1hvda9", link_info = ContentInfo {author = "yotsubakoiwai", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 1, downs = 0, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Nothing, num_comments = 1, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hvda9/a_drawing_of_a_pokeball/", saved = False, score = 1, selftext = Nothing, thumbnail = "http://a.thumbs.redditmedia.com/V-_LlyQ7qfXcK9dZ.jpg", url = "http://i.imgur.com/NIn7ZDA.jpg"},Link {link_rname = "t3_1hvba5", link_info = ContentInfo {author = "Ignorant_Amurican", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 11, downs = 1, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Nothing, num_comments = 1, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hvba5/crappy_kids_artwork/", saved = False, score = 10, selftext = Nothing, thumbnail = "http://e.thumbs.redditmedia.com/1GiGjuT-BSyiUTsL.jpg", url = "http://i.imgur.com/wrtZ0oq.jpg"},Link {link_rname = "t3_1hvabq", link_info = ContentInfo {author = "necessary_fart_noise", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 3, downs = 1, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Nothing, num_comments = 2, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hvabq/blurry_kid_with_banana/", saved = False, score = 2, selftext = Nothing, thumbnail = "http://c.thumbs.redditmedia.com/bJojl1RleO70eD0t.jpg", url = "http://i.imgur.com/9j90VAx.jpg"},Link {link_rname = "t3_1hv0v2", link_info = ContentInfo {author = "Caliga", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 19, downs = 0, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Nothing, num_comments = 1, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hv0v2/an_old_dog/", saved = False, score = 19, selftext = Nothing, thumbnail = "http://e.thumbs.redditmedia.com/kYS-F_tLvAQcF3e4.jpg", url = "http://i.imgur.com/Rdfe7tr.jpg"},Link {link_rname = "t3_1huwd4", link_info = ContentInfo {author = "Grasswalker", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 3, downs = 1, likes = Nothing}, clicked = False, domain = "imgur.com", hidden = False, is_self = False, link_flair = Nothing, num_comments = 1, link_over_18 = False, permalink = "/r/no_sob_story/comments/1huwd4/a_shirt/", saved = False, score = 2, selftext = Nothing, thumbnail = "http://c.thumbs.redditmedia.com/EPlyexXWq2097MUh.jpg", url = "http://imgur.com/U7mAuHD"},Link {link_rname = "t3_1huv7v", link_info = ContentInfo {author = "Asmaedus", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 10, downs = 4, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Nothing, num_comments = 3, link_over_18 = False, permalink = "/r/no_sob_story/comments/1huv7v/3_books/", saved = False, score = 6, selftext = Nothing, thumbnail = "default", url = "http://i.imgur.com/IH0spaZ.jpg"},Link {link_rname = "t3_1hutzz", link_info = ContentInfo {author = "Ell223", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 3, downs = 2, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Nothing, num_comments = 1, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hutzz/a_keyring/", saved = False, score = 1, selftext = Nothing, thumbnail = "http://f.thumbs.redditmedia.com/TQdRztJIR8X_fodA.jpg", url = "http://i.imgur.com/mRtR5CNh.jpg"},Link {link_rname = "t3_1hud0j", link_info = ContentInfo {author = "kiwimark", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 7, downs = 1, likes = Nothing}, clicked = False, domain = "reddit.com", hidden = False, is_self = False, link_flair = Nothing, num_comments = 5, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hud0j/and_another_normal_looking_house/", saved = False, score = 6, selftext = Nothing, thumbnail = "default", url = "http://www.reddit.com/r/pics/comments/1hu3cy/as_a_single_father_and_working_15_years_to/"},Link {link_rname = "t3_1hucma", link_info = ContentInfo {author = "kiwimark", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 16, downs = 2, likes = Nothing}, clicked = False, domain = "imgur.com", hidden = False, is_self = False, link_flair = Nothing, num_comments = 1, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hucma/just_some_normal_old_wedding_photos/", saved = False, score = 14, selftext = Nothing, thumbnail = "http://e.thumbs.redditmedia.com/9CLu4GxHyF0FS8iL.jpg", url = "http://imgur.com/a/q71o3#kHzRJch"},Link {link_rname = "t3_1hu096", link_info = ContentInfo {author = "attheoffice", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 2, downs = 1, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 1, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hu096/dyson_airblade_710/", saved = False, score = 1, selftext = Nothing, thumbnail = "http://d.thumbs.redditmedia.com/JblvNcINkNr_OUrc.jpg", url = "http://i.imgur.com/PZt6fKU.jpg"},Link {link_rname = "t3_1htv5o", link_info = ContentInfo {author = "lolzforfun", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 27, downs = 6, likes = Nothing}, clicked = False, domain = "imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 3, link_over_18 = False, permalink = "/r/no_sob_story/comments/1htv5o/girl_before_and_after_haircut/", saved = False, score = 21, selftext = Nothing, thumbnail = "http://d.thumbs.redditmedia.com/_CJiBEP81WQbO7zE.jpg", url = "http://imgur.com/a/EuJ0H"},Link {link_rname = "t3_1htuu0", link_info = ContentInfo {author = "YesSirSir", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 27, downs = 7, likes = Nothing}, clicked = False, domain = "imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 4, link_over_18 = False, permalink = "/r/no_sob_story/comments/1htuu0/a_taco_bell_hot_sauce_packet/", saved = False, score = 20, selftext = Nothing, thumbnail = "http://c.thumbs.redditmedia.com/8wuroTNmnuUaWr_2.jpg", url = "http://imgur.com/IleSZaA"},Link {link_rname = "t3_1htqul", link_info = ContentInfo {author = "crocodilekyle55", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 8, downs = 4, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 2, link_over_18 = False, permalink = "/r/no_sob_story/comments/1htqul/street_sign/", saved = False, score = 4, selftext = Nothing, thumbnail = "http://b.thumbs.redditmedia.com/Edvx5g3y6dUB0CKy.jpg", url = "http://i.imgur.com/DholtOr.jpg"},Link {link_rname = "t3_1htp61", link_info = ContentInfo {author = "slipknot6477", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 24, downs = 3, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 2, link_over_18 = False, permalink = "/r/no_sob_story/comments/1htp61/a_house/", saved = False, score = 21, selftext = Nothing, thumbnail = "http://c.thumbs.redditmedia.com/aysuWud7aLH-urJV.jpg", url = "http://i.imgur.com/YVsfeIg.jpg"},Link {link_rname = "t3_1htfrj", link_info = ContentInfo {author = "redditnotfacebook", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 4, downs = 6, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Nothing, num_comments = 1, link_over_18 = False, permalink = "/r/no_sob_story/comments/1htfrj/a_house/", saved = False, score = 0, selftext = Nothing, thumbnail = "http://b.thumbs.redditmedia.com/n6xlG3cVXNvloCHy.jpg", url = "http://i.imgur.com/9JcJ6la.jpg"},Link {link_rname = "t3_1htbw0", link_info = ContentInfo {author = "DrHuzaifa", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 80, downs = 9, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 4, link_over_18 = False, permalink = "/r/no_sob_story/comments/1htbw0/a_house/", saved = False, score = 71, selftext = Nothing, thumbnail = "http://b.thumbs.redditmedia.com/n6xlG3cVXNvloCHy.jpg", url = "http://i.imgur.com/9JcJ6la.jpg"},Link {link_rname = "t3_1ht5wm", link_info = ContentInfo {author = "crocodilekyle55", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 30, downs = 2, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 3, link_over_18 = False, permalink = "/r/no_sob_story/comments/1ht5wm/man_holding_broken_glass/", saved = False, score = 28, selftext = Nothing, thumbnail = "http://c.thumbs.redditmedia.com/yDXpp3Mhu1kaXJdP.jpg", url = "http://i.imgur.com/J7TV58C.jpg"},Link {link_rname = "t3_1ht5m1", link_info = ContentInfo {author = "crocodilekyle55", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 2, downs = 1, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 1, link_over_18 = False, permalink = "/r/no_sob_story/comments/1ht5m1/kitten/", saved = False, score = 1, selftext = Nothing, thumbnail = "http://f.thumbs.redditmedia.com/ak5yFS8vNfQay6Re.jpg", url = "http://i.imgur.com/gw9QrQ4.jpg"},Link {link_rname = "t3_1hsv00", link_info = ContentInfo {author = "papavoikos", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 23, downs = 2, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 5, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hsv00/guy_with_pizza/", saved = False, score = 21, selftext = Nothing, thumbnail = "http://b.thumbs.redditmedia.com/UbZKODZRWQkA8jgC.jpg", url = "http://i.imgur.com/uPM18Vd.jpg"},Link {link_rname = "t3_1hsuht", link_info = ContentInfo {author = "papavoikos", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 32, downs = 8, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 6, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hsuht/marshmallow/", saved = False, score = 24, selftext = Nothing, thumbnail = "http://e.thumbs.redditmedia.com/JCPy6TlAXMrpd-QF.jpg", url = "http://i.imgur.com/ZhRW10P.jpg"},Link {link_rname = "t3_1hso4t", link_info = ContentInfo {author = "kiwimark", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 14, downs = 6, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 3, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hso4t/a_dog/", saved = False, score = 8, selftext = Nothing, thumbnail = "http://c.thumbs.redditmedia.com/JPzAtDEftTym3P4J.jpg", url = "http://i.imgur.com/xor7KbG.jpg"},Link {link_rname = "t3_1hsna1", link_info = ContentInfo {author = "flamu", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 8, downs = 2, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 1, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hsna1/man_holding_sign/", saved = False, score = 6, selftext = Nothing, thumbnail = "http://f.thumbs.redditmedia.com/f2Z8eOKgcNF0OTvM.jpg", url = "http://i.imgur.com/aL5UuAP.jpg"},Link {link_rname = "t3_1hsn3d", link_info = ContentInfo {author = "dafood", author_flair = Nothing, edited = False, subreddit = "no_sob_story", subreddit_id = "t5_2vgps", distinguished = Nothing}, link_votes = Votes {ups = 14, downs = 2, likes = Nothing}, clicked = False, domain = "i.imgur.com", hidden = False, is_self = False, link_flair = Just (Left ""), num_comments = 3, link_over_18 = False, permalink = "/r/no_sob_story/comments/1hsn3d/food_on_a_shelve_sob_story/", saved = False, score = 12, selftext = Nothing, thumbnail = "http://d.thumbs.redditmedia.com/m5M1bonCK_1HZXV9.jpg", url = "http://i.imgur.com/vQxBoGm.jpg"}]
