/*  MAGEEC LLVM Plugin
    Copyright (C) 2013,2014 Embecosm Limited and University of Bristol

    This file is part of MAGEEC.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>. */

#ifndef LLVM_TESTMAGEEC_H
#define LLVM_TESTMAGEEC_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/Pass.h"
#include "mageec/mageec.h"

using namespace mageec;

namespace llvm {

class MageecInterface {
  static mageec_framework framework;
  static bool initialized;

  // Decide whether to run a Pass based on its name.
  bool decideName(std::string pass, std::string function);

  void initializeFramework();
public:
  MageecInterface();
  ~MageecInterface();

  // Decide whether to run a FunctionPass
  bool decideFP(const Pass *pass, const Function &F);
  // Decide whether to run a ModulePass
  bool decideMP(const Pass *pass, const Module &M);
};

} // end namespace llvm

#endif
