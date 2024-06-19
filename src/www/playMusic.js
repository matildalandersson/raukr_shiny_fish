var audio;

Shiny.addCustomMessageHandler('playMusic', function(message) {
  if (!audio) {
    audio = new Audio('sound-of-a-quiet-spring-stream-background-sounds-of-nature-144993.mp3');
    audio.loop = true;
    audio.volume = message.volume; // Set the default volume
  }
  
  if (audio.paused) {
    audio.play();
  }
});

Shiny.addCustomMessageHandler('stopMusic', function(message) {
  if (audio) {
    audio.pause();
    audio.currentTime = 0; // Reset audio to start
    audio = null; // Reset the audio object
  }
});