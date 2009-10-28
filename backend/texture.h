#include <GL/gl.h>
#include <GL/glu.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <stdlib.h>
#include <stdio.h>

struct render_spec {
  float texture_rect[4];
  float quad_points[4];
  float center_ratio[2];
  GLuint list_cache;
  int w;
  int h;
};


struct texture {
  GLuint name;
  struct render_spec spec;

};

// ####################################### render spec fuctions #######################################
struct render_spec * make_render_spec(int w, int h);


struct render_spec * set_center(struct render_spec *spec, int w_ratio, int h_ratio);


struct render_spec * recenter(struct render_spec *spec);


inline int cachedp(struct render_spec * spec);

struct render_spec * cache(struct render_spec * spec);

void render_render_spec(struct render_spec * spec);
  


// ####################################### texture functions #######################################


inline int loadedp(struct texture * tex);


struct texture * make_texture(GLuint name, struct render_spec * spec);
