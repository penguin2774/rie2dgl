#include "backend.h"



struct image * make_image(struct texture *tex, float x, float y, float z, float scale, float rot)
{
  
  struct image * result = malloc(sizeof(struct image));
  result->tex = tex;
  result->loc[0] = x;
  result->loc[1] = y;
  result->loc[2] = z;
  result->w = tex->spec->w;
  result->h = tex->spec->h;
  result->scale = scale;
  result->rot = rot;
  result->spec = 0;
  result->subs = 0;
  return result;
}


void free_image(struct image *img)
{
  // Freeing a image frees the sub images. 
  union sprite * i;
  while(i = pop_subimage(img)) 
    free_sprite(i);
  if(img->spec)
    free_render_spec(img->spec);
  free(img);
}



struct image * cache_image(struct image *image)
{
  if(image->spec)
    cache(image->spec);
  else
    cache(image->tex->spec);
  return image;
}

void render_image(struct image *img)
{
  float * loc = img->loc;
  float rot = img->rot;
  float scale = img->scale;
  struct sprite_cons * sp;
  glEnable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glColor3f (0.0, 0.0, 0.0);
  glPushMatrix();
  glTranslatef(loc[0], loc[1], loc[2]);
  if(rot)
    glRotatef(rot, 0.0, 0.0, 1.0);
  
  if(scale != 1.0)
    glScalef(scale, scale, 1.0);
  
  glBindTexture( GL_TEXTURE_2D, img->tex->name);
  
  if(img->spec)
    render_render_spec(img->spec);
  else
    render_render_spec(img->tex->spec);
  for(sp = img->subs; sp != 0; sp = sp->next) {
    render(sp->data);
  }
  glPopMatrix();
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
}

struct image * set_image_center(struct image * image, float w_ratio, float h_ratio)
{
  if(!image->spec) {
    image->spec = make_render_spec( image->tex->spec->w, image->tex->spec->h);
  }
  set_center(image->spec, w_ratio, h_ratio);
  return image;
}

    
struct image * push_subimage(struct image * image, union sprite *sub)
{
  struct sprite_cons * new_cell = malloc(sizeof(struct sprite_cons));
  new_cell->next = image->subs;
  image->subs = new_cell;
  new_cell->data = sub;
  return image;
}
// no way of checking success...
struct image * rem_subimage(struct image * image, union sprite *sub)
{
  struct sprite_cons * i, * prev;
  prev =  0;
  for(i = image->subs; i != 0; i = i->next) {
    if (i->data == sub) {
      if(prev)
	prev->next = i->next;
      else
	image->subs = i->next;
      free(i);

    }
  }
  return image;
}

union sprite * pop_subimage(struct image * image)
{
  if (image->subs) {
    struct sprite_cons * cell = image->subs;
    union sprite * result = cell->data;
    image->subs = cell->next;
    free(cell);
    return result;
  }
  else
    return 0;
}

inline struct image * rotate_image(struct image * image, float radians)
{
  image->rot = radians;
  return image;
}


inline struct image * scale_image(struct image * image, float scale)
{
  image->scale = scale;
  return image;
}

inline struct image * move_image(struct image * image, float x, float y, float z)
{
  image->loc[0] += x;
  image->loc[1] += y;
  image->loc[2] += z;
  return image;
}

inline struct image * relocate_image(struct image * image, float x, float y, float z)
{
  image->loc[0] = x;
  image->loc[1] = y;
  image->loc[2] = z;
  return image;
}


struct image * change_image_texture(struct image * image, struct texture * texture)
{
  if(image->spec) {
    struct render_spec * spec = image->spec;

    spec->w = texture->spec->w;
    spec->h = texture->spec->h;
    recenter(spec);
  }
  image->tex = texture;
  return image;
}
