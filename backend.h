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
  struct render_spec *spec;

};

struct sprite {
  enum sprite_type {
  SPRITE_IMAGE,
  SPRITE_ANIMATION} type;
  union sprite_union {
    struct image * image;
    struct animation * anim;
    void * raw;
  } data;
};

struct sprite_cons {
  struct sprite * data;
  struct sprite_cons * next;
};

struct image {
  struct texture * tex;
  float loc[3];
  struct render_spec * spec;
  float scale;
  float rot;
  int w, h;
  struct sprite_cons * subs;
};

#define ANIM_STOPPED  1<<0
#define ANIM_LOOP     1<<1

struct animation {
  struct image * image;
  struct texture ** frames;
  float ticks;
  float frame_rate;
  int frame_count;
  int current_frame;
  int flags;
};


// ####################################### render spec fuctions ###############
struct render_spec * make_render_spec(int w, int h);


struct render_spec * set_center(struct render_spec *spec, float w_ratio, float h_ratio);


struct render_spec * recenter(struct render_spec *spec);


inline int cachedp(struct render_spec * spec);

struct render_spec * cache(struct render_spec * spec);
struct render_spec * free_cache(struct render_spec * spec);

void render_render_spec(struct render_spec * spec);
  
inline void free_render_spec(struct render_spec * spec);

// ####################################### texture functions ##################


inline int loadedp(struct texture * tex);


struct texture * make_texture(GLuint name, struct render_spec * spec);

inline void free_texture(struct texture * tex);


// ####################################### image functions 

struct image * make_image(struct texture *tex, float x, float y, float z, float scale, float rot);

void free_image(struct image *img);

struct image * cache_image(struct image *img);


void render_image(struct image *img);




struct image * set_image_center(struct image * image, float w_ratio, float h_ratio);


    
struct image * push_subimage(struct image * image, struct sprite *sub);

// no way of checking success...
struct image * rem_subimage(struct image * image, struct sprite *sub);

struct sprite * pop_subimage(struct image * image);


inline struct image * rotate_image(struct image * image, float radians);


inline struct image * scale_image(struct image * image, float scale);

inline struct image * move_image(struct image * image, float x, float y, float z);

inline struct image * relocate_image(struct image * image, float x, float y, float z);


struct image * change_image_texture(struct image * image, struct texture * texture);
    

// #################################### Animation Fuctions

struct animation * make_animation(struct texture ** texs, unsigned int count, float frame_rate, long flags, float x, float y, float z, float scale, float rot);

void free_animation(struct animation * anim);

inline int stoppedp(struct animation *anim);

inline int last_framep(struct animation * anim);

inline struct animation  * toggle(struct animation * anim);

inline struct animation  * start(struct animation * anim);

inline struct animation  * stop(struct animation * anim);

inline struct animation  * reset_ticks(struct animation * anim);

struct animation  * change_frames(struct animation * anim, struct texture ** texs, int count);

struct animation  * next_frame(struct animation * anim);

struct animation  * prev_frame(struct animation * anim);

struct animation  * set_frame(struct animation * anim, unsigned int frame);

struct animation  * first_frame(struct animation * anim);

struct animation  * last_frame(struct animation * anim);

void render_animation(struct animation * anim);

// #################################### Sprite Functions


void free_sprite(struct sprite *);

void render_sprite(struct sprite * sprite);

void render_sprites (struct sprite ** sprites, unsigned int count);
