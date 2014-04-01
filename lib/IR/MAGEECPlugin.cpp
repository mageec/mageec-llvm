/*  MAGEEC LLVM Plugin/Interface
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

#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/MAGEECPlugin.h"

// FIXME: We shouldn't include iostream and should use debug interface instead
#include <iostream>

namespace llvm {

mageec::mageec_framework MageecInterface::framework;
bool MageecInterface::initialized = false;

MageecInterface::MageecInterface() {
}

MageecInterface::~MageecInterface() {
  if (!initialized)
    return;
  framework.finish();
  initialized = false;
}

bool MageecInterface::decideFP(const Pass *pass, const Function &F) {
  initializeFramework();
  std::string FuncName = F.getName();
  std::string PassName = pass->getPassName();

  bool decision = decideName(PassName, FuncName);
  std::cout << " Fn, \"" << FuncName << "\", \"" << pass->getPassName()
       << "\"" << std::endl;
  return decision;
}

bool MageecInterface::decideMP(const Pass *pass, const Module &M) {
  initializeFramework();
  std::string FuncName = "*NOFN*";
  std::string PassName = pass->getPassName();

  bool decision = decideName(PassName, FuncName);
  std::cout << "Mod, \"" << M.getModuleIdentifier() << "\", \""
       << pass->getPassName() << "\"" << std::endl;
  return decision;
}

bool MageecInterface::decideName(std::string pass, std::string function) {
  // There are some special cases where we want to bypass the usual
  // decision making process and make a hard-coded decision.
  // For example we always want to run a FunctionPassManager to have
  // access to FunctionPasses.
  bool SpecialCase = StringSwitch<bool>(pass)
    .Case("Function Pass Manager", true)
    .Case("Loop Pass Manager", true)
    .Default(false);
  if (SpecialCase)
    return true;

  mageec::decision d = framework.make_decision(pass, function);

  switch (d) {
    // At this stage on LLVM, the native decision is always to run
    default:
    case mageec::NATIVE_DECISION:
      return true;
    case mageec::FORCE_EXECUTE:
      return true;
    case mageec::FORCE_NOEXECUTE:
      return false;
  }
}

void MageecInterface::initializeFramework() {
  if (initialized)
    return;

  initialized = !framework.init("", "");
  assert(initialized && "Unable to initilize MAGEEC framework.");
}

}
