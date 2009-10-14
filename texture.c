#include "backend.h"


struct render_spec * make_render_spec(int w, int h)
{
  struct render_spec * result = malloc(sizeof(struct render_spec));
  result->w = w;
  result->h = h;
  result->texture_rect[0] = result->texture_rect[1] = 0.0;
  result->texture_rect[2] = result->texture_rect[3] = 1.0;
  result->list_cache = 0;
  set_center(result, 0.5, 0.5);
  
  return result;
}

struct render_spec * clone_render_spec(struct render_spec * spec)
{
  struct render_spec * result = malloc(sizeof (struct render_spec));
  result->w = spec->w;
  result->h = spec->h;
  result->texture_rect[0] = spec->texture_rect[0];
  result->texture_rect[1] = spec->texture_rect[1];
  result->texture_rect[2] = spec->texture_rect[2];
  result->texture_rect[3] = spec->texture_rect[3];
  result->quad_points[0] = spec->quad_points[0]; 
  result->quad_points[1] = spec->quad_points[1]; 
  result->quad_points[2] = spec->quad_points[2]; 
  result->quad_points[3] = spec->quad_points[3];
  result->list_cache = 0;
  result->center_ratio[0] = spec->center_ratio[0];
  result->center_ratio[1] = spec->center_ratio[1];
  return result;
}

struct render_spec * set_center(struct render_spec *spec, float w_ratio, float h_ratio)
{
  spec->center_ratio[0] = w_ratio;
  spec->center_ratio[1] = h_ratio;
  recenter(spec);
}

struct render_spec * recenter(struct render_spec *spec)
{
  // recentering a cached spec kills the cache
  if(cachedp(spec))
     free_cache(spec);
  spec->quad_points[0] = spec->w * spec->center_ratio[0] - spec->w;
  spec->quad_points[1] = spec->h * spec->center_ratio[1] - spec->h;
  spec->quad_points[2] = spec->w * spec->center_ratio[0];
  spec->quad_points[3] = spec->h * spec->center_ratio[1];
}

enum flip_axises {
  FLIP_H,
  FLIP_V,
  FLIP_HV
};

struct render_spec * flip_spec(struct render_spec *spec, enum flip_axises dir)
{
  float store;
  switch(dir) {
  case FLIP_H:
    store = spec->texture_rect[0];
    spec->texture_rect[0] = spec->texture_rect[2];
    spec->texture_rect[2] = store;
    break;
  case FLIP_V:
    store = spec->texture_rect[1];
    spec->texture_rect[1] = spec->texture_rect[3];
    spec->texture_rect[3] = store;
    break;
  case FLIP_HV:
    store = spec->texture_rect[0];
    spec->texture_rect[0] = spec->texture_rect[2];
    spec->texture_rect[2] = store;
    store = spec->texture_rect[1];
    spec->texture_rect[1] = spec->texture_rect[3];
    spec->texture_rect[3] = store;
    break;
  }
  return spec;
}
    
  
  

inline int cachedp(struct render_spec * spec)
{
  if(spec->list_cache == 0)
    return 0;
  else
    return 1;
}


void render_render_spec(struct render_spec * spec)
{
  GLuint name = glGenLists(1);
  float * trect = spec->texture_rect;
  float * qpoints = spec->quad_points;

  if(cachedp(spec)) {
    
    glCallList(spec->list_cache);
  }
  else {

    glBegin(GL_QUADS);

  // texture points are reversed on the y axis to flip the image
  // cause SDL stores top to bottom and GL reads in reverse.

    glTexCoord2f(trect[0], trect[3]);
    glVertex3f(qpoints[0], qpoints[1], 0.0);
    
    glTexCoord2f(trect[2], trect[3]);
    glVertex3f(qpoints[2], qpoints[1], 0.0);
    
    glTexCoord2f(trect[2], trect[1]);
    glVertex3f(qpoints[2], qpoints[3], 0.0);
    
    glTexCoord2f(trect[0], trect[1]);
    glVertex3f(qpoints[0], qpoints[3], 0.0);
    
    glEnd();

  }
}

struct render_spec * cache(struct render_spec * spec)
{
  GLuint name = glGenLists(1);
  float * trect = spec->texture_rect;
  float * qpoints = spec->quad_points;
  glNewList(name, GL_COMPILE);
  glBegin(GL_QUADS);

  // texture points are reversed on the y axis to flip the image
  // cause SDL stores top to bottom and GL reads in reverse.

  glTexCoord2f(trect[0], trect[3]);
  glVertex3f(qpoints[0], qpoints[1], 0.0);

  glTexCoord2f(trect[2], trect[3]);
  glVertex3f(qpoints[2], qpoints[1], 0.0);

  glTexCoord2f(trect[2], trect[1]);
  glVertex3f(qpoints[2], qpoints[3], 0.0);

  glTexCoord2f(trect[0], trect[1]);
  glVertex3f(qpoints[0], qpoints[3], 0.0);
  
  glEnd();

  glEndList();
  spec->list_cache = name;
}


struct render_spec * free_cache(struct render_spec * spec)
{
  glDeleteLists(spec->list_cache, 1);
  spec->list_cache = 0;
}


inline int loadedp(struct texture * tex)
{
  if(tex->name && glIsTexture(tex->name))
    return 1;
  else
    return 0;
}

struct texture * make_texture(GLuint name, struct render_spec * spec)
{
  struct texture *result = malloc (sizeof(struct texture));
  result->name = name;
  result->spec = spec;
  return result;
}

enum texture_clone_op {
  TEXTURE_NOOP,
  TEXTURE_FLIP_H,
  TEXTURE_FLIP_V,
  TEXTURE_FLIP_HV
};
  


struct texture * clone_texture(struct texture * tex, enum texture_clone_op op)
{
  struct texture * result = malloc(sizeof(struct texture));

  result->name = tex->name;
  result->spec = clone_render_spec(tex->spec);
  
  switch(op){
  case TEXTURE_NOOP:
    break;
  case TEXTURE_FLIP_H:
    flip_spec(result->spec, FLIP_H);
    break;
  case TEXTURE_FLIP_V:
    flip_spec(result->spec, FLIP_V);
    break;
  case TEXTURE_FLIP_HV:
    flip_spec(result->spec, FLIP_HV);
    break;
  }
  return result;
}

    
inline void free_render_spec(struct render_spec * spec)
{
  free(free_cache(spec));
}

inline void free_texture(struct texture * tex)
{
  free_render_spec(tex->spec);
  GLuint texture[1];
  if(loadedp(tex)) {
    texture[0] = tex->name;
    glDeleteTextures(1, texture);
  }
  free(tex);
}

