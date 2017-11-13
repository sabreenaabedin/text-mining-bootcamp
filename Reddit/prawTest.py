import praw
import django

from django.utils.encoding import smart_str, smart_unicode

reddit = praw.Reddit(client_id='5BjtAtxEWG2KOw',
                     client_secret="46LxAZQpnwqNVZeNfzzLPC6LyLY", password='!Weather26',
                     user_agent='testOne', username='tss3dn')
#print(reddit.user.me())



#replace 'acne' with the subreddit you want to scrape!
#for submission in reddit.subreddit('popping').hot(limit=10):
   #print(submission.title)

subredditName = "acne"
wordOfInterest = "lemon"

for submission in reddit.subreddit(subredditName).hot(limit=2500):
   for top_level_comment in submission.comments:
       print smart_str(top_level_comment.body + ';')


      # if wordOfInterest in (top_level_comment.body):










                       #+ ','+ str(top_level_comment.score) + ','+ str(top_level_comment.ups) + ',' + str(top_level_comment.downs))


#acne
#SkincareAddiction
#asianbeauty
#SkincareAddicts
#Accutane
#popping
#Skincare_Addiction