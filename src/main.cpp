/*
 * unplusplus
 * Copyright 2021 Eric Eaton
 */

#include <iostream>
#include <irrlicht/irrlicht.h>
#include <ecl/ecl.h>

using namespace irr;
using namespace core;
using namespace scene;
using namespace video;
using namespace io;
using namespace gui;

extern "C" {
extern void init_lib_IRRLISP(cl_object);
}

static int swank_port;

void init_lisp(int argc, char *argv[], int port) {
  cl_object result;
  cl_boot(argc, argv);
  // TODO: Find a way to handle SIGFPE
  // ecl_set_option(ECL_OPT_TRAP_SIGFPE, 0);
  result = cl_eval(
      c_string_to_object("(ext:set-signal-handler ext:+SIGINT+ #'ext:quit)"));
  ecl_init_module(NULL, init_lib_IRRLISP);
  swank_port = port;

  const cl_env_ptr l_env = ecl_process_env();
  CL_CATCH_ALL_BEGIN(l_env) {
    CL_UNWIND_PROTECT_BEGIN(l_env) {
      result = cl_eval(c_string_to_object(
          ("(swank:create-server :port " + std::to_string(swank_port) + " :dont-close t)").c_str()));
      uint64_t port = ecl_to_uint64_t(result);
      std::cout << "SWANK active on port: " << port << std::endl;
    }
    CL_UNWIND_PROTECT_EXIT {
      std::cout << "Protected region exit" << std::endl;
    }
    CL_UNWIND_PROTECT_END;
  }
  CL_CATCH_ALL_IF_CAUGHT { std::cout << "Error caught" << std::endl; }
  CL_CATCH_ALL_END;
}

void deinit_lisp() {
  cl_object result = cl_eval(c_string_to_object(
      ("(swank:stop-server " + std::to_string(swank_port) + ")").c_str()));
  cl_shutdown();
}

int main(int argc, char *argv[]) {
  init_lisp(argc, argv, 8117);

  IrrlichtDevice *device =
      createDevice( video::EDT_OPENGL, dimension2d<u32>(1920, 1080), 16,
                    false, false, true, 0);

  if (!device)
    return 1;

  device->setWindowCaption(L"Hello World! - Irrlicht Engine Demo");

  IVideoDriver* driver = device->getVideoDriver();
  ISceneManager* smgr = device->getSceneManager();
  IGUIEnvironment* guienv = device->getGUIEnvironment();

  guienv->addStaticText(L"Hello World! This is Irrlicht with the opengl renderer!",
                        rect<s32>(10,10,300,32), true);

  smgr->addCameraSceneNode(0, vector3df(0,30,-40), vector3df(0,5,0));

  cl_object result = cl_eval(c_string_to_object("(make-sn)"));
  ecl_print(result->foreign.tag, ECL_T);

  while(device->run())
  {
    /*
      Anything can be drawn between a beginScene() and an endScene()
      call. The beginScene() call clears the screen with a color and
      the depth buffer, if desired. Then we let the Scene Manager and
      the GUI Environment draw their content. With the endScene()
      call everything is presented on the screen.
    */
    driver->beginScene(true, true, SColor(255,100,101,140));

    smgr->drawAll();
    guienv->drawAll();

    driver->endScene();
  }

  device->drop();

  deinit_lisp();

  return 0;
}
