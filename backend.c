#include <GL/gl.h>
#include <GL/glu.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <stdlib.h>
#include <stdio.h>

  
struct texture {
  int w,h;
  GLuint name;
};

struct sprite {
 
  float x,y;
  struct texture * tex;
};

void load_texture(char * filename, struct texture *tex)
{
  SDL_Surface * surface;
  GLuint textures[1];
  surface = IMG_Load(filename);
  glGenTextures(1, textures);
  glBindTexture(GL_TEXTURE_2D, textures[0]);
  
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, surface->w, surface->h,
	       0, GL_RGBA, GL_UNSIGNED_BYTE, surface->pixels);
  tex->w = surface->w;
  tex->h = surface->h;
  tex->name = textures[0];
  SDL_FreeSurface(surface);

}



void setup_RC () 
{
  glClearColor(0.0, 0.0, 0.0, 0.0);
 
}



void render_sprite(struct sprite *sprite)
{
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glColor3f(1.0, 1.0, 1.0);
  glBindTexture( GL_TEXTURE_2D, sprite->tex->name);
  glPushMatrix();
  glTranslatef(sprite->x, sprite->y, 0.0);
  //  glScalef(1.0, 200.0, 0.0);
  
  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 1.0);
  glVertex3f(0.0, 0.0, 0.0);

  glTexCoord2f(1.0, 1.0);
  glVertex3f((float) sprite->tex->w, 0.0, 0.0);

  glTexCoord2f(1.0, 0.0);
  glVertex3f((float) sprite->tex->w, (float) sprite->tex->h, 0.0);

  glTexCoord2f(0.0, 0.0);
  glVertex3f(0.0, (float) sprite->tex->h, 0.0);
  glEnd();
  glPopMatrix();
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
}

void resize_window(int w, int h)
{
  if( h == 0)
    h = 1;
  glViewport(0,0, w, h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0.0, (float) w, 0.0, (float) h);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

/* #define SPRITES 10000 */


void render_sprites(struct sprite *sprites, int count)
{
  int i;
  for(i = 0; i < count; i++) { 
    render_sprite(&(sprites[i])); 
  } 
}
  

/* int main(int argc, char ** argv) */
/* { */
/*   SDL_Surface * display; */
/*   SDL_Event event; */
/*   struct texture tex; */
/*   struct sprite sprites[SPRITES]; */
/*   int i; */
/*   SDL_Init(SDL_INIT_EVERYTHING); */
/*   display = SDL_SetVideoMode(512, 512, 0, SDL_OPENGLBLIT); */
/*   SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1); */
/*   resize_window(512, 512); */
/*   setup_RC(); */
/*   load_texture(&tex); */
 /*   for(i = 0; i < SPRITES; i++) { */
/*     sprites[i].tex = &tex; */
/*     sprites[i].x =(float) (rand() % 512); */
/*     sprites[i].y =(float) (rand() % 512); */

/*   } */
 


/*   do { */
/*     //glClear( GL_COLOR_BUFFER_BIT); */
/*     for(i = 0; i < SPRITES; i++) { */
/*       render_sprite(&(sprites[i])); */
/*     } */
    
/*     glFlush(); */
/*     SDL_GL_SwapBuffers(); */
/*     SDL_Delay(22); */
/*     SDL_PollEvent(&event); */
/*   } while(event.type != SDL_QUIT); */
/*   SDL_Quit(); */
/*   return 0; */
/* } */
    
      
  
    
