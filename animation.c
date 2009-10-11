#include "backend.h"



struct animation * make_animation(struct texture ** texs, unsigned int count, float frame_rate, long flags, float x, float y, float z, float scale, float rot)
{
  struct animation * result = malloc(sizeof(struct texture));
  result->image = make_image(texs[0], x, y, z, scale, rot);
  result->frames = texs;
  result->frame_count = count;
  result->current_frame = 0;
  result->flags = flags;
  return result;
}

void free_animation(struct animation * anim)
{
  free_image(anim->image);
  free(anim);
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

struct animation  * next_frame(struct animation * anim)
{
  if(++anim->current_frame >= anim->frame_count)
    anim->current_frame = 0;
  reset_ticks(anim);
  change_image_texture(anim->image, anim->frames[anim->current_frame]);
  return anim;
}

struct animation  * prev_frame(struct animation * anim)
{
  if(--anim->current_frame < 0)
    anim->current_frame = anim->frame_count - 1;
  reset_ticks(anim);
  change_image_texture(anim->image, anim->frames[anim->current_frame]);
  return anim;
}


//  fails silently if frame is out of range.
struct animation  * set_frame(struct animation * anim, unsigned int frame)
{
  if(frame >= anim->frame_count)
    return anim;
  reset_ticks(anim);
  anim->current_frame = frame;
  change_image_texture(anim->image, anim->frames[frame]);
  return anim;
}

struct animation  * first_frame(struct animation * anim)
{
  reset_ticks(anim);
  anim->current_frame = 0;
  change_image_texture(anim->image, anim->frames[0]);
  return anim;
}

struct animation  * last_frame(struct animation * anim)
{
  int last = anim->frame_count - 1;
  reset_ticks(anim);
  anim->current_frame = last;
  change_image_texture(anim->image, anim->frames[last]);
  return anim;
}

inline int last_framep(struct animation * anim)
{
  return anim->current_frame - 1 == anim->frame_count;
}


void render_animation(struct animation * anim)
{
  if(!(anim->flags & ANIM_STOPPED)) {
    
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

      
    
    
    

