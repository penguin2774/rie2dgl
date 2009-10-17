#include "backend.h"


struct animation * make_disabled_animation(struct texture * texs, float x, float y, float z, float scale, float rot)
{
  struct animation * result = malloc(sizeof(struct animation));
  result->image = make_image(texs, x, y, z, scale, rot);
  result->ticks = 0.0;
  result->frames = 0;
  result->frame_count = 0;
  result->frame_rate = 1;
  result->current_frame = 0;
  result->flags = 0;
  return result;
}



struct animation * make_animation(struct texture ** texs, unsigned int count, float frame_rate, long flags, float x, float y, float z, float scale, float rot)
{
  struct animation * result = malloc(sizeof(struct animation));
  result->image = make_image(texs[0], x, y, z, scale, rot);
  result->ticks = 0.0;
  result->frames = texs;
  result->frame_count = count;
  result->frame_rate = frame_rate;
  result->current_frame = 0;
  result->flags = flags;
  return result;
}


void free_animation(struct animation * anim)
{
  free_image(anim->image);
  free(anim);
}

inline int animation_disabledp(struct animation *ptr)
{
  return(!(ptr->frames)); // animation is disabled if there are no frames.
}

inline int stoppedp(struct animation *anim)
{
  return(anim->flags & ANIM_STOPPED);
}

inline struct animation  * toggle(struct animation * anim)
{
  anim->flags ^= ANIM_STOPPED;
  return anim;
}

inline struct animation  * start(struct animation * anim)
{
  if(stoppedp(anim))
    toggle(anim);
  return anim;
}

inline struct animation  * stop(struct animation * anim)
{
  if(!stoppedp(anim))
     toggle(anim);
  return(anim);
}

inline struct animation  * reset_ticks(struct animation * anim)
{
  anim->ticks = 0;
  return anim;
}

struct animation  * change_frames(struct animation * anim, struct texture ** texs, int count)
{
  anim->frames = texs;
  anim->frame_count = count;
  anim->ticks = 0;
  change_image_texture(anim->image, texs[0]);
  return anim;
}

struct animation * disable_animation(struct animation *anim)
{
  anim->frames = 0;
  anim->frame_count = 0;
  anim->ticks = 0;
  anim->flags = 0;
  return anim;
}

struct animation  * next_frame(struct animation * anim)
{
  if(!animation_disabledp(anim)) {
      if(++anim->current_frame >= anim->frame_count)
	anim->current_frame = 0;
      reset_ticks(anim);
      change_image_texture(anim->image, anim->frames[anim->current_frame]);
    }
  return anim;
}

struct animation  * prev_frame(struct animation * anim)
{
  if(!animation_disabledp(anim)){
    if(--anim->current_frame < 0)
      anim->current_frame = anim->frame_count - 1;
    reset_ticks(anim);
    change_image_texture(anim->image, anim->frames[anim->current_frame]);
  }
  return anim;
}


//  fails silently if frame is out of range.
struct animation  * set_frame(struct animation * anim, unsigned int frame)
{
  if(!animation_disabledp(anim)) {
    if(frame >= anim->frame_count)
      return anim;
    reset_ticks(anim);
    anim->current_frame = frame;
    change_image_texture(anim->image, anim->frames[frame]);
  }
  return anim;
}

struct animation  * first_frame(struct animation * anim)
{
  if(!animation_disabledp(anim)) {
    reset_ticks(anim);
    anim->current_frame = 0;
    change_image_texture(anim->image, anim->frames[0]);
  }
  return anim;
}

struct animation  * last_frame(struct animation * anim)
{
  if(!animation_disabledp(anim)) {
    int last = anim->frame_count - 1;
    reset_ticks(anim);
    anim->current_frame = last;
    change_image_texture(anim->image, anim->frames[last]);
  }
  return anim;
}

inline int last_framep(struct animation * anim)
{
  if(!animation_disabledp(anim)) {
    return anim->current_frame - 1 == anim->frame_count;
  }
  else
    return 0;
}


void render_animation(struct animation * anim)
{
  if(animation_disabledp(anim) && !(anim->flags & ANIM_STOPPED)) {
    
    anim->ticks += anim->frame_rate;
    
    if(anim->ticks >= 1) {
      
      anim->ticks = 0;
      next_frame(anim);
      if(anim->current_frame + 1 >= anim->frame_count && !(anim->flags & ANIM_LOOP))
	stop(anim);
      
    }
  
  }
  render_image(anim->image);
}

      
    
    
    

