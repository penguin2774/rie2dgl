#include "backend.h"


struct sprite * make_sprite(void * data, enum sprite_type type) 
{
  struct sprite * sprite = malloc(sizeof(struct sprite));
  sprite->data.raw = data;
  sprite->type = type;
  return sprite;
}
  
   
 

void free_sprite(struct sprite * sprite)
{
  switch(sprite->type) {
  case SPRITE_IMAGE:
    free_image(sprite->data.image);
    break;
  case SPRITE_ANIMATION:
    free_animation(sprite->data.anim);
    break;
  }
  free(sprite);
}


void render_sprite(struct sprite * sprite)
{
  switch(sprite->type) {
  case SPRITE_IMAGE:

    render_image(sprite->data.image);
    break;
  case SPRITE_ANIMATION:

    render_animation(sprite->data.anim);
    break;
  }
}

void render_sprites(struct sprite ** sprites, unsigned int count)
{
  int i;

  for(i = 0; i < count; i++) {
     render_sprite(sprites[i]);
  }
}
