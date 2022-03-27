/*
 * unplusplus
 * Copyright 2021 Eric Eaton
 */

#include <iostream>
#include <irrlicht.h>
#include <ecl/ecl.h>
#include "all_lisp_systems.h"

// library includes
#include <IrrIMGUI/IncludeIrrlicht.h>
#include <IrrIMGUI/IncludeIMGUI.h>
#include <IrrIMGUI/IrrIMGUI.h>
#include <IrrIMGUI/IrrIMGUIDebug.h>

using namespace irr;
using namespace core;
using namespace scene;
using namespace video;
using namespace io;
using namespace gui;
using namespace IrrIMGUI;

static int swank_port;

void init_lisp(int argc, char *argv[], int port) {
  cl_object result;
  cl_boot(argc, argv);
  // TODO: Find a way to handle SIGFPE
  // ecl_set_option(ECL_OPT_TRAP_SIGFPE, 0);
  result = cl_eval(
      c_string_to_object("(ext:set-signal-handler ext:+SIGINT+ #'ext:quit)"));
  ecl_init_all_modules();
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

IrrlichtDevice *device = nullptr;
IGUIStaticText *text = nullptr;

extern "C" void gui_debugger(cl_object condition, cl_object old_hook) {
  std::cout << "GUI debugger" << std::endl;
  cl_object restarts = cl_compute_restarts(1, condition);
  cl_object str = cl_prin1_to_string(restarts);
  if (ECL_BASE_STRING_P(str)) {
    std::wstring s((char *)str->string.self,
                   (char *)str->string.self + str->string.dim);
    text->setText(s.c_str());
  } else if (ECL_EXTENDED_STRING_P(str)) {
    std::wstring s((wchar_t *)str->string.self,
                   (wchar_t *)str->string.self + str->string.dim);
    text->setText(s.c_str());
  }
}

int main(int argc, char *argv[]) {
  // Create standard event receiver for the IrrIMGUI
  CIMGUIEventReceiver EventReceiver;

  // Irrlicht Settings
  SIrrlichtCreationParameters IrrlichtParams;
  IrrlichtParams.DriverType    = video::EDT_OPENGL;
  IrrlichtParams.WindowSize    = core::dimension2d<u32>(1920, 1080);
  IrrlichtParams.Bits          = 32;
  IrrlichtParams.Fullscreen    = false;
  IrrlichtParams.Stencilbuffer = true;
  IrrlichtParams.AntiAlias     = 16;
  IrrlichtParams.Vsync         = true;
  IrrlichtParams.EventReceiver = &EventReceiver;

  device = createDeviceEx(IrrlichtParams);
  if (!device)
    return 1;

  // Create GUI object
  IIMGUIHandle * const pGUI = createIMGUI(device, &EventReceiver);

  device->setWindowCaption(L"Hello World! - Irrlicht Engine Demo");

  IVideoDriver* driver = device->getVideoDriver();
  ISceneManager* smgr = device->getSceneManager();
  IGUIEnvironment* guienv = device->getGUIEnvironment();

  text = guienv->addStaticText(L"Hello World! This is Irrlicht with the opengl renderer!",
                        rect<s32>(10,10,300,32), true);

  smgr->addCameraSceneNode(0, vector3df(0, 30, -40), vector3df(0, 5, 0));

  init_lisp(argc, argv, 8117);

  cl_env_ptr env = ecl_process_env();
  ECL_CATCH_ALL_BEGIN(env) {
    cl_object result;
    result = cl_set(ecl_make_symbol("*DEVICE*", "IRRLISP-TEST"), ecl_make_pointer(device));
    ecl_print(result, ECL_T);
    result = cl_set(ecl_make_symbol("*DRIVER*", "IRRLISP-TEST"), ecl_make_pointer(driver));
    ecl_print(result, ECL_T);
    result = cl_funcall(1, ecl_make_symbol("SETUP", "IRRLISP-TEST"));
    ecl_print(result, ECL_T);
    std::cout << std::endl;
  } ECL_CATCH_ALL_IF_CAUGHT {
    std::cout << "There was an error running lisp" << std::endl;
  } ECL_CATCH_ALL_END;

  while(device->run())
  {
    /*
      Anything can be drawn between a beginScene() and an endScene()
      call. The beginScene() call clears the screen with a color and
      the depth buffer, if desired. Then we let the Scene Manager and
      the GUI Environment draw their content. With the endScene()
      call everything is presented on the screen.
    */
    driver->beginScene(ECBF_COLOR | ECBF_DEPTH, SColor(255, 100, 101, 140));

    pGUI->startGUI();

    ECL_CATCH_ALL_BEGIN(env) {
      cl_funcall(1, ecl_make_symbol("IMGUI", "IRRLISP-TEST"));
    } ECL_CATCH_ALL_IF_CAUGHT {
      std::cout << "There was an error running lisp" << std::endl;
    } ECL_CATCH_ALL_END;

    smgr->drawAll();
    guienv->drawAll();
    pGUI->drawAll();

    driver->endScene();
  }

  device->drop();
  pGUI->drop();

  deinit_lisp();

  return 0;
}
