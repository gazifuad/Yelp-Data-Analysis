library(beeswarm)
library(ggpubr)

load(file="/Users/cjj4/Downloads/yelp reviews cities/yelp_port_reviews_users.rda")

port_plot<-ggplot(yelp_port_reviews_users,
                  aes(x=factor(stars.x),
                      y=review_length,
                      color=factor(elite_dum)))+
  geom_quasirandom(alpha=0.75, size=num_comp)+
  theme_minimal()+
  xlab("Stars")+
  ylab("Review Length")+
  ggtitle("Review Length by Stars, Elite Member, and Number of Compliments on Reviews",subtitle = "Reviews from Portland, Oregon")+
  scale_color_discrete(name="Elite Users",type=c("tan3","navy"),labels=c("Regular User","Elite"))+
  scale_size_continuous(name="Number of Compliments on Review")

load(file="/Users/cjj4/Downloads/yelp reviews cities/yelp_boul_reviews_users.rda")

boul_plot<-ggplot(yelp_boul_reviews_users,
                  aes(x=factor(stars.x),
                      y=review_length,
                      color=factor(elite_dum)))+
  geom_quasirandom(alpha=0.75, size=num_comp)+
  theme_minimal()+
  xlab("Stars")+
  ylab("Review Length")+
  ggtitle("Review Length by Stars, Elite Member, and Number of Compliments on Reviews",subtitle = "Reviews from Boulder, Colorado")+
  scale_color_discrete(name="Elite Users",type=c("tan3","navy"),labels=c("Regular User","Elite"))+
  scale_size_continuous(name="Number of Compliments on Review")

load(file="/Users/cjj4/Downloads/yelp reviews cities/yelp_col_reviews_users.rda")

col_plot<-ggplot(yelp_col_reviews_users,
                  aes(x=factor(stars.x),
                      y=review_length,
                      color=factor(elite_dum)))+
  geom_quasirandom(alpha=0.75, size=num_comp)+
  theme_minimal()+
  xlab("Stars")+
  ylab("Review Length")+
  ggtitle("Review Length by Stars, Elite Member, and Number of Compliments on Reviews",subtitle = "Reviews from Columbus, Ohio")+
  scale_color_discrete(name="Elite Users",type=c("tan3","navy"),labels=c("Regular User","Elite"))+
  scale_size_continuous(name="Number of Compliments on Review")

load(file="/Users/cjj4/Downloads/yelp reviews cities/yelp_atl_reviews_users.rda")

atl_plot<-ggplot(yelp_atl_reviews_users,
                  aes(x=factor(stars.x),
                      y=review_length,
                      color=factor(elite_dum)))+
  geom_quasirandom(alpha=0.75, size=num_comp)+
  theme_minimal()+
  xlab("Stars")+
  ylab("Review Length")+
  ggtitle("Review Length by Stars, Elite Member, and Number of Compliments on Reviews",subtitle = "Reviews from Atlanta, Georgia")+
  scale_color_discrete(name="Elite Users",type=c("tan3","navy"),labels=c("Regular User","Elite"))+
  scale_size_continuous(name="Number of Compliments on Review")

load(file="/Users/cjj4/Downloads/yelp reviews cities/yelp_aus_reviews_users.rda")

aus_plot<-ggplot(yelp_aus_reviews_users,
                  aes(x=factor(stars.x),
                      y=review_length,
                      color=factor(elite_dum)))+
  geom_quasirandom(alpha=0.75, size=num_comp)+
  theme_minimal()+
  xlab("Stars")+
  ylab("Review Length")+
  ggtitle("Review Length by Stars, Elite Member, and Number of Compliments on Reviews",subtitle = "Reviews from Austin, Texas")+
  scale_color_discrete(name="Elite Users",type=c("tan3","navy"),labels=c("Regular User","Elite"))+
  scale_size_continuous(name="Number of Compliments on Review")

load(file="/Users/cjj4/Downloads/yelp reviews cities/yelp_orl_reviews_users.rda")

orl_plot<-ggplot(yelp_orl_reviews_users,
                  aes(x=factor(stars.x),
                      y=review_length,
                      color=factor(elite_dum)))+
  geom_quasirandom(alpha=0.75, size=num_comp)+
  theme_minimal()+
  xlab("Stars")+
  ylab("Review Length")+
  ggtitle("Review Length by Stars, Elite Member, and Number of Compliments on Reviews",subtitle = "Reviews from Orlando, Florida")+
  scale_color_discrete(name="Elite Users",type=c("tan3","navy"),labels=c("Regular User","Elite"))+
  scale_size_continuous(name="Number of Compliments on Review")

load(file="/Users/cjj4/Downloads/yelp reviews cities/yelp_bos_reviews_users.rda")

bos_plot<-ggplot(yelp_bos_reviews_users,
                  aes(x=factor(stars.x),
                      y=review_length,
                      color=factor(elite_dum)))+
  geom_quasirandom(alpha=0.75, size=num_comp)+
  theme_minimal()+
  xlab("Stars")+
  ylab("Review Length")+
  ggtitle("Review Length by Stars, Elite Member, and Number of Compliments on Reviews",subtitle = "Reviews from Boston, Massachussetts")+
  scale_color_discrete(name="Elite Users",type=c("tan3","navy"),labels=c("Regular User","Elite"))+
  scale_size_continuous(name="Number of Compliments on Review")

ggarrange(boul_plot,port_plot,orl_plot,aus_plot,atl_plot,col_plot,bos_plot)

