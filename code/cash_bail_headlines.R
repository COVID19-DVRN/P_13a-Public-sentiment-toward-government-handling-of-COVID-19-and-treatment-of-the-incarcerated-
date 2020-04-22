library(tidyverse)
library(stringdist)
library(rvest)

raw_stories_no_covid <- read_csv(here::here("data", "cumulative cash bail no covid story urls.csv"),
                        col_types = cols(
                          stories_id = col_double(),
                          publish_date = col_datetime(format = "%m/%d/%Y %H:%M"),
                          title = col_character(),
                          url = col_character(),
                          language = col_character(),
                          ap_syndicated = col_logical(),
                          themes = col_character(),
                          media_id = col_double(),
                          media_name = col_character(),
                          media_url = col_character()
                        )) %>%
  mutate(file = "cumulative cash bail no covid story urls.csv")

raw_stories <- read_csv(here::here("data", "cumulative cash bail story urls.csv"),
                        col_types = cols(
                          stories_id = col_double(),
                          publish_date = col_datetime(format = "%m/%d/%Y %H:%M"),
                          title = col_character(),
                          url = col_character(),
                          language = col_character(),
                          ap_syndicated = col_logical(),
                          themes = col_character(),
                          media_id = col_double(),
                          media_name = col_character(),
                          media_url = col_character()
                        )) %>%
  mutate(file = "cumulative cash bail story urls.csv")

raw_stories <- full_join(raw_stories_no_covid, raw_stories, 
                         by = c("stories_id", "publish_date", 
                                "title", "url", "language", "ap_syndicated", 
                                "themes", "media_id", "media_name", "media_url"))

# mediacloud sources
files <- list.files(here::here("data", "sources"),
                    full.names = TRUE, pattern = "*.csv")

sources <- map_dfr(files, read_csv,
                   .id = "source_group",
                   col_types = cols(
                     .default = col_character(),
                     media_id = col_double(),
                     editor_notes = col_logical(),
                     stories_per_day = col_double(),
                     first_story = col_date(format = "")
                   )) %>%
  mutate(source_group = factor(
    source_group, labels = map_chr(files, ~ str_split(., "\\(|\\)")[[1]][2])
  )) %>%
  arrange(source_group) %>%
  group_by_at(vars(-source_group)) %>%
  filter(row_number() == 1) %>% # some sources belong to > 1 group
  ungroup()

# merge with stories
stories <- raw_stories %>%
  distinct() %>% # remove any initial duplicates (none)
  left_join(sources, by = "media_id", suffix = c("", "_source")) %>%
  mutate(
    date = lubridate::date(publish_date),
    raw_title = str_to_lower(title) %>%
      str_remove_all("[[:punct:]]") %>%
      str_squish()
  )

stories_no_dupes <- stories %>%
  group_by(raw_title) %>% # remove duplicate titles w/out punctuation, capitals
  arrange(-ap_syndicated, source_group) %>%
  mutate(n = n(), duplicate = n() > 1) %>%
  filter(row_number() == 1) %>%
  ungroup()

# calculate a measure of distance between titles
title_dist <- stringdistmatrix(stories_no_dupes$raw_title, stories_no_dupes$raw_title,
                              method = "lcs", useNames = "strings")

# kind of normalize it be number of characters
# (just ad hoc for later checking manually)
total_chars <- outer(stories_no_dupes$raw_title, stories_no_dupes$raw_title,
                    FUN = function(x, y) nchar(paste(x, y)))

dist_meas <- title_dist / total_chars

# the upper bound is arbitrary
poss_dupes <- apply(dist_meas, 1, function(x) any(between(x, 1e-12, .05)))

# these can now be filtered manually
stories_no_dupes <- stories_no_dupes %>%
 mutate(poss_dupe = poss_dupes)

# same news source with different urls, or reported at the exact same time
# left in others that weren't from the same source and posted at slightly different times
duplicate_stories <- c(
  1221383338, 1236247116, 1503596450, 1230784162, 1239841475,
  1236912331, 1532884548, 1181500976, 1172022793, 605323993, 
  1498847812, 1298474367, 826172229, 1484194571,
  657168505, 804324704
)

stories_no_dupes <- stories_no_dupes %>%
  filter(!stories_id %in% duplicate_stories)

# attempt to download html from url
# commented out and read in when already downloaded
safe_full_text <- possibly(read_html, otherwise = NA)

full_texts_html <- map(stories_no_dupes$url, safe_full_text)
safe_write_xml <- possibly(write_xml, otherwise = NA)
walk2(full_texts_html, stories_no_dupes$stories_id, # not writing directly to project directory??
      ~safe_write_xml(.x, file = str_glue("~/Google Drive/Projects/COVID/Project13 - Jails:Prisons/data/cash_bail_full",
                                                   "/text_{.y}.xml")))

full_texts_html <- map(stories_no_dupes$stories_id, 
                       ~safe_full_text(here::here("data", "cash_bail_full",
                                                  str_glue("text_{.}.xml"))))

# extract the p elements -- generally have the article text
safe_get_text <- possibly(~xml_text(xml_find_all(.x, "//p")), otherwise = NA)

full_texts <- map(full_texts_html, safe_get_text)

stories_no_dupes <- stories_no_dupes %>%
  mutate(full_text = map_chr(full_texts, ~ reduce(., paste, .init = "")))

work <- stories_no_dupes %>%
  select(date, media_name, url, raw_title, full_text) %>%
  mutate(
    text = str_remove_all(full_text, "(\\n)|(\\t)") %>%
      str_to_lower() %>%
      str_remove_all("©|°|\\$|~|\\|") %>%
      str_remove_all("[0-9]") %>%
      str_squish(),
    first_letters = str_sub(text, 1, 15),
    last_letters = str_sub(text, nchar(text) - 15, nchar(text))
  )

to_remove <- work %>%
  group_by(first_letters) %>%
  mutate(n = n()) %>%
  filter(row_number() == 1) %>%
  arrange(desc(n))

to_remove2 <- work %>%
  group_by(last_letters) %>%
  mutate(n = n()) %>%
  filter(row_number() == 1) %>%
  arrange(desc(n))

strings_to_remove <- c( # manually extracted from the beginning or end of the stories that share beginnings/ends
  "have an existing account? already have a subscription? don't have an account? get the news let friends in your social network know what you are reading about",
  "a link has been sent to your friends email address a link has been posted to your facebook feed to find out more about facebook commenting please read the conversation guidelines and faqs welcome to our new and improved comments which are for subscribers only this is a test to see whether we can improve the experience for you you do not need a facebook profile to participate you will need to register before adding a comment typed comments will be lost if you are not logged in please be polite its ok to disagree with someones ideas but personal attacks insults threats hate speech advocating violence and other violations can result in a ban if you see comments in violation of our community guidelines please report them with help from the cdc we answer some of googles most searched questions about the coronavirus crisis",
  "we invite you to use our commenting platform to engage in insightful conversations about issues in our community although we do not prescreen comments we reserve the right at all times to remove any information or materials that are unlawful threatening abusive libelous defamatory obscene vulgar pornographic profane indecent or otherwise objectionable to us and to disclose any information necessary to satisfy the law regulation or government request we might permanently block any user who abuses these conditions if you see comments that you find offensive please use the flag as inappropriate feature by hovering over the right side of the post and pulling down on the arrow that appears or contact our editors by emailing moderatorscngcom this website uses cookies to improve your experience by continuing to use the site you accept our privacy policy and cookie policy",
  "get breaking news in your browser. click here to turn on notifications",
  "notifications can be turned off anytime in the browser settings",
  "check the status of the virus in your state with your state health departments websites by tapping below download the brand new wusa app here sign up for the get up dc newsletter your forecast your commute your news",
  "settings cancel set",
  "thanks for contacting us. we've received your submission",
  "would you like to receive local news notifications on your desktop",
  "watch videos",
  " ad choices",
  "(about us)",
  "sign in manage newsletters",
  "filed under",
  "not a member? register",
  "this material may not be published, broadcast, rewritten, or redistributed. fox news network, llc. all rights reserved",
  "all market data delayed minutes",
  "fox news flash top headlines are here check out whats clicking on foxnewscom get all the latest news on coronavirus and more delivered daily to your inbox sign up here",
  "support us about us, contact us, staff, careers, circulation, privacy, terms",
  "help or sign in with a social account: don't have an account yet? sign up › get the most out of your experience with a personalized all-access pass to everything local on events, music, restaurants, news and more. enter your email or sign up with a social account to get started already registered? login",
  "the lens in-depth news and investigations for new orleans",
  "this content is being provided for free as a public service to our readers during the coronavirus outbreak",
  "sign in to your forbes account or register for instructions on how to disable your ad blocker, click here. if this is your first time registering, please check your inbox for more information about the benefits of your forbes account and what you can do next",
  "rigorous nonprofit news for vermont",
  "watch cbsn live by",
  "help us keep reporting",
  "triblive's daily and weekly email newsletters deliver the news you want and information you need, right to your inbox",
  "all rights reserved",
  "free daily headlines",
  "dear readers, we need your help. the coronavirus crisis in portland is a major threat to the mercury's ability to keep the city informed. we pride ourselves on having navigated many storms in the world of independent local media, but this time is different. % of our revenue—from advertising, ticketing fees, and our own events—is directly tied to people getting together in groups. the coronavirus situation has virtually eliminated this income all at once. at a time when the city needs local coverage more than ever, we're asking for your help to support continued coverage or everything happening in portland. you can make one-time or recurring donations. we can't say enough how much we appreciate your support. thank you",
  "dear readers, the coronavirus pandemic has caused widespread disruption to the lives of everyone in tampa bay and to so many businesses in our community. here at the tampa bay times, we continue to provide free, up-to-date information at tampabay.com/coronavirus as a public service. but we need your help. please consider supporting us by subscribing or donating, and by sharing our work. thank you",
  "subscribe donate newsletters editor’s note: the salt lake tribune is providing readers free access to critical local stories about the coronavirus during this time of heightened concern. see more coverage here",
  "get the latest news delivered daily we invite you to use our commenting platform to engage in insightful conversations about issues in our community although we do not prescreen comments we reserve the right at all times to remove any information or materials that are unlawful threatening abusive libelous defamatory obscene vulgar pornographic profane indecent or otherwise objectionable to us and to disclose any information necessary to satisfy the law regulation or government request we might permanently block any user who abuses these conditions if you see comments that you find offensive please use the flag as inappropriate feature by hovering over the right side of the post and pulling down on the arrow that appears or contact our editors by emailing moderatorscngcom this website uses cookies to improve your experience by continuing to use the site you accept our privacy policy and cookie policy",
  "usa today network choose the plan thats right for you digital access or digital and print delivery",
  "your california privacy rights privacy policy gannett",
  "do not sell my personal information cookie policy do not sell my personal information privacy policy terms of service",
  "your california privacy rights / privacy policy gannett usa today network",
  "choose the plan thats right for you. digital access or digital and print delivery",
  "original content available for noncommercial use under a creative commons license except where noted",
  "hearst television participates in various affiliate marketing programs, which means we may get paid commissions on purchases made through our links to retailer sites",
  "all rights reservedterms of useprivacy noticeyour ad choicessitemapcalifornia privacy rightsdo not sell my personal information would you like to receive desktop browser notifications about breaking news and other major stories? not nowyes please",
  "you must log in to post a comment",
  "note to readers: if you purchase something through one of our affiliate links we may earn a commission",
  "registration on or use of this site constitutes acceptance of our user agreement, privacy policy and cookie statement, and your california privacy rights",
  "advance local media llc. all rights reserved (about us). the material on this site may not be reproduced, distributed, transmitted, cached or otherwise used, except with the prior written permission of advance local. community rules apply to all content you upload or otherwise submit to this site. ad choices",
  "get up-to-the-minute news sent straight to your device",
  "get up to speed with our essential california newsletter, sent six days a week",
  "sign up for the latest news, best stories and what they mean for you, plus answers to your questions",
  "subscribe for unlimited access",
  "follow the latest on the outbreak with our newsletter every weekday all stories in the newsletter are free to access by signing up you agree to our terms of use and privacy policy follow the latest on the outbreak with our newsletter every weekday all stories in the newsletter are free to access by signing up you agree to our terms of use and privacy policy",
  "click here to access the online public inspection file viewers with disabilities can get assistance accessing this station's fcc public inspection file by contacting the station with the information listed below. questions or concerns relating to the accessibility of the fcc's online public file system should be directed to the fcc",
  "view the discussion thread",
  "accessibility tools",
  "readers around grass valley and nevada county make the unions work possible your financial contribution supports our efforts to deliver quality locally relevant journalism now more than ever your support is critical to help us keep our community informed about the evolving coronavirus pandemic and the impact it is having locally every contribution however large or small will make a difference your donation will help us continue to cover covid and our other vital local news get immediate access to organizations and people in our area that need your help or can provide help during the coronavirus crisis start a dialogue stay on topic and be civil if you dont follow the rules your comment may be deleted card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone work for the best boss you invest hrs daily and make an extra seeking a contractor for an inground fiberglass spa install andor sq ft building enclosure real estate agents save covid = stress destresser = no fees functiond s id var jsijs=dgetelementsbytagnamesifdgetelementbyididreturnjs=dcreateelementsjsid=idjssrc=embedscribblelivecomwidgetsembedjsijsparentnodeinsertbeforejs ijsdocument script scrbbljs thuh l frih l sath l sunh l monh l card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone card vfcounter vfcommentscountdisplaynone classifieds jobs real estate rentals autos business service directory pets photos for sale merchandise garage sales contact us contribute subscribe subscriber services about us comment policy advertise newsletter signup magazines cookie list sierra sun tahoe daily tribune tahoecom the wildwood independent swift communications inc",
  "want the latest news and weather updates",
  "watch live",
  "copyright the associated press",
  "the associated press contributed to this report",
  "quotes delayed at least minutes. real-time quotes provided by bats bzx real-time price. market data provided by interactive data (terms & conditions). powered and implemented by interactive data managed solutions. company fundamental data provided by morningstar. earnings estimates data provided by zacks. mutual fund and etf data provided by lipper. economic data provided by econoday. dow jones & company terms & conditions. this material may not be published, broadcast, rewritten, or redistributed. fox news network, llc. all rights reserved. faq - updated privacy policy",
  "this material may not be published, broadcast, rewritten or redistributed",
  "start a dialogue, stay on topic and be civil",
  "if you don't follow the rules, your comment may be deleted",
  "classifieds jobs real estate rentals autos business & service directory pets photos for sale merchandise garage sales contact us contribute subscribe subscriber services about us comment policy advertise newsletter signup magazines cookie list sierra sun tahoe daily tribune tahoe.com the wildwood independent - swift communications, inc",
  "you must log in to post a comment",
  "this website uses cookies to improve your experience. by continuing to use the site, you accept our privacy policy and cookie policy",
  "do not sell my personal information",
  "cookie policy",
  "privacy policy",
  "terms of service",
  "wilmington tv. . contact@wilm-tv.com capitol broadcasting company wilm-tv  terms of use fcc/eeo reportsite developed and hosted by impact media solutions",
  "for more information, go to",
  "sign up for our newsletters",
  "associated press and may not be published, broadcast, rewritten, or redistributed. associated press text, photo, graphic, audio and/or video material shall not be published, broadcast, rewritten for broadcast or publication or redistributed directly or indirectly in any medium. neither these ap materials nor any portion thereof may be stored in a computer except for personal and noncommercial use. the ap will not be held liable for any delays, inaccuracies, errors or omissions therefrom or in the transmission or delivery of all or any part thereof or for any damages arising from any of the foregoing",
  "would you like to receive desktop browser notifications about breaking news and other major stories",
  "choose the plan that’s right for you. digital access or digital and print delivery",
  "original content available for non-commercial use under a creative commons license, except where noted",
  "check back later for updates to this story. get morning report and other email newsletters",
  "the material on this site may not be reproduced, distributed, transmitted, cached or otherwise used, except with the prior written permission of advance local",
  "community rules apply to all content you upload or otherwise submit to this site",
  "join our facebook group for the latest updates on coronavirus",
  "x trending:",
  "you are now logged in. forgot your password? create new account",
  "accessibility terms of use",
  "get morning report and other email newsletters",
  "this story has been shared",
  "not nowyes please",
  "manage newsletters",
  "all stories in the newsletter are free to access",
  "by signing up you agree to our terms of use",
  "click to read more and view comments",
  "click to hide terms of use",
  "washington weather summary: degrees washington",
  "to keep our community informed of the most urgent coronavirus news, our critical updates are free to read. for more in-depth coverage and analysis, subscribe now",
  "already a subscriber?",
  "sign up now to get the most recent coronavirus headlines and other important local and national news sent to your email inbox daily",
  "log in or activate your account",
  "have an upcoming event? click below to share it with the community! plus, after your event is approved, log back into your user dashboard for an opportunity to enhance your listing",
  "get the latest local and national news",
  "please log in, or sign up for a new account and purchase a subscription to continue reading",
  "the best local, regional and national news in sports, politics, business and more",
  "on your next view you will be asked to",
  "subscribe today for unlimited access",
  "if you're a current print subscriber, you can opt-in for all access at any time",
  "sorry, an error occurred",
  "we hope that you enjoy our free content",
  "thank you for reading",
  "if you previously used a social network to login to wral.com, click the “forgot your password” link to reset your password",
  "orry, no promotional deals were found matching that code",
  "please subscribe or activate your digital account today",
  "stories about the coronavirus pandemic are free to read as a public service",
  "if this coverage is important to you, consider supporting local journalism by subscribing",
  "follow the latest on the outbreak with our newsletter every weekday",
  "please donate to keep us thriving through this crisis and beyond",
  "become a donor and go ad-free",
  "get the latest updates in news, food, music and culture, and receive special offers direct to your inbox",
  "get the latest news delivered daily! we invite you to use our commenting platform to engage in insightful conversations about issues in our community. although we do not pre-screen comments, we reserve the right at all times to remove any information or materials that are unlawful, threatening, abusive, libelous, defamatory, obscene, vulgar, pornographic, profane, indecent or otherwise objectionable to us, and to disclose any information necessary to satisfy the law, regulation, or government request. we might permanently block any user who abuses these conditions. if you see comments that you find offensive, please use the \"flag as inappropriate\" feature by hovering over the right side of the post, and pulling down on the arrow that appears. or, contact our editors by emailing",
  "get social working for tips are you a covid- expert, public health worker, medical provider, elected official, employer, business owner, or patient? we’d like to include your expertise, data, experiences, concerns, or anonymous tips related to covid- in our reporting. click to connect with our newsroom"
)
# for exact matching
strings_to_remove_coll <- map(strings_to_remove, coll)

# remove those strings and try again to see if anything else pops up
work <- work %>%
  mutate(
    new_text = reduce(strings_to_remove_coll, str_remove_all, .init = text),
    first_letters = str_sub(new_text, 1, 40),
    last_letters = str_sub(new_text, nchar(new_text) - 40, nchar(new_text))
  )

# check again
to_remove <- work %>%
  group_by(first_letters) %>%
  mutate(n = n()) %>%
  filter(row_number() == 1) %>%
  arrange(desc(n))

to_remove2 <- work %>%
  group_by(last_letters) %>%
  mutate(n = n()) %>%
  filter(row_number() == 1) %>%
  arrange(desc(n))


# revised text is in column new_text
stories_no_dupes <- left_join(stories_no_dupes, work)

write_rds(stories_no_dupes, here::here("data", "cash_bail_full.rds"))
write_csv(stories_no_dupes, here::here("data", "cash_bail_full.csv"))
