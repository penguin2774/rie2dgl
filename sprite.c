#include "backend.h"


void free_sprite(union sprite * sprite)
{
  switch(sprite->type) {
  case SPRITE_IMAGE:
    free_image(sprite->image);
    break;
  case SPRITE_ANIMATION:
    free_animation(sprite->anim);
    break;
  }
}


void render(union sprite * sprite)
{
  switch(sprite->type) {
  case SPRITE_IMAGE:
    render_image(sprite->image);
    break;
  case SPRITE_ANIMATION:
    render_animation(sprite->anim);
    break;
  }
}
