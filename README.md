# TerminalTok

A terminal-native social media platform supporting ASCII art, terminal-rendered videos, and real-time encrypted messaging, powered by a hybrid recommendation engine.

Contributors:
Ruslan Black,
Camden Keller,
Scott Fukuda,
Rishik Gowrishetti,
Ruben Lemmon

## Vision

Our vision was to build a terminal-native social media platform that supports both ASCII art posts and terminal-rendered videos. The platform includes core interactions such as liking, scrolling, and secure chatting, as well as a recommendation system that adapts based on user engagement.

Since MS1, the project has expanded in several meaningful ways. We added ASCII art posts, which fit naturally with the terminal aesthetic, and introduced an offline mode featuring our custom dinosaur game to give users a fun break from scrolling.

Since MS2, our vision has continued to evolve as we ideated around the user experience. We significantly enhanced the dinosaur game, adding a variety of entirely original features that differentiate it from the Google Chrome version. We also made our machine learning recommendation system far more robust: it now incorporates content popularity among similar users in addition to individual engagement. To enable this, we added user history logs, created detailed metadata for all content, and implemented watch-time tracking. Lastly, we added support for videos rendered in the terminal.

## Summary of Progress

Online mode has two versions: ASCII art display and in-terminal display. For the ASCII art display, users can view various camels as well as choose to like or unlike them. For the in-terminal video display, users can watch, pause, and skip various short videos. While this occurs, our recommendation algorithm works in the background to tailor content to the user's interests.

Our recommendation system is a hybrid model that ranks videos by combining three weighted signals:

- **Content-Based (30%):** Matches videos to genres watched frequently.
- **Collaborative Filtering (35%):** Finds similar users and recommends what they liked.
- **ML Personalization (35%):** Uses matrix factorization to predict interest based on latent hidden patterns.

For new users with no history, the system defaults to a random selection to jumpstart the learning process.

Additionally, we have implemented a chat system and logging. The chat system enables different users to chat from separate terminal windows on the same device through our custom server. The logging keeps track of what content a user has seen, their like/unlike status, and their watch time. Messages sent from the client to the server are encrypted using the Diffie-Hellman algorithm.

Offline mode is inspired by Chrome’s dinosaur game, played directly in the terminal. The character, represented by a string, must jump over obstacles (via frontflip, regular jump, or backflip commands) or slide under them. Obstacles appear as "67" strings at random, yet evadable, intervals. Players can also deploy an F-16 that charges over time; once available, it flies across the screen and drops a "67 bomb" to temporarily destroy all obstacles. The game includes a day/night mode that switches every 500 points, tracks the user’s score, and displays a message upon losing.
