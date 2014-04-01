//===-- Analysis.cpp ------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm-c/Analysis.h"
#include "llvm-c/Initialization.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/InitializePasses.h"
#include "llvm/PassRegistry.h"
#include <cstring>

using namespace llvm;

/// initializeAnalysis - Initialize all passes linked into the Analysis library.
void llvm::initializeAnalysis(PassRegistry &Registry) {
  initializeAliasAnalysisAnalysisGroup(Registry);
  initializeAliasAnalysisCounterPass(Registry);
  initializeAAEvalPass(Registry);
  initializeAliasDebuggerPass(Registry);
  initializeAliasSetPrinterPass(Registry);
  initializeNoAAPass(Registry);
  initializeBasicAliasAnalysisPass(Registry);
  initializeBlockFrequencyInfoPass(Registry);
  initializeBranchProbabilityInfoPass(Registry);
  initializeCostModelAnalysisPass(Registry);
  initializeCFGViewerPass(Registry);
  initializeCFGPrinterPass(Registry);
  initializeCFGOnlyViewerPass(Registry);
  initializeCFGOnlyPrinterPass(Registry);
  initializeDependenceAnalysisPass(Registry);
  initializeDelinearizationPass(Registry);
  initializeDominanceFrontierPass(Registry);
  initializeDomViewerPass(Registry);
  initializeDomPrinterPass(Registry);
  initializeDomOnlyViewerPass(Registry);
  initializePostDomViewerPass(Registry);
  initializeDomOnlyPrinterPass(Registry);
  initializePostDomPrinterPass(Registry);
  initializePostDomOnlyViewerPass(Registry);
  initializePostDomOnlyPrinterPass(Registry);
  initializeIVUsersPass(Registry);
  initializeInstCountPass(Registry);
  initializeIntervalPartitionPass(Registry);
  initializeLazyValueInfoPass(Registry);
  initializeLibCallAliasAnalysisPass(Registry);
  initializeLintPass(Registry);
  initializeLoopInfoPass(Registry);
  initializeMemDepPrinterPass(Registry);
  initializeMemoryDependenceAnalysisPass(Registry);
  initializeModuleDebugInfoPrinterPass(Registry);
  initializePostDominatorTreePass(Registry);
  initializeRegionInfoPass(Registry);
  initializeRegionViewerPass(Registry);
  initializeRegionPrinterPass(Registry);
  initializeRegionOnlyViewerPass(Registry);
  initializeRegionOnlyPrinterPass(Registry);
  initializeScalarEvolutionPass(Registry);
  initializeScalarEvolutionAliasAnalysisPass(Registry);
  initializeTargetTransformInfoAnalysisGroup(Registry);
  initializeTypeBasedAliasAnalysisPass(Registry);
  initializeMageecFeatureExtractorPass(Registry);
}

void LLVMInitializeAnalysis(LLVMPassRegistryRef R) {
  initializeAnalysis(*unwrap(R));
}

LLVMBool LLVMVerifyModule(LLVMModuleRef M, LLVMVerifierFailureAction Action,
                          char **OutMessages) {
  raw_ostream *DebugOS = Action != LLVMReturnStatusAction ? &errs() : 0;
  std::string Messages;
  raw_string_ostream MsgsOS(Messages);

  LLVMBool Result = verifyModule(*unwrap(M), OutMessages ? &MsgsOS : DebugOS);

  // Duplicate the output to stderr.
  if (DebugOS && OutMessages)
    *DebugOS << MsgsOS.str();

  if (Action == LLVMAbortProcessAction && Result)
    report_fatal_error("Broken module found, compilation aborted!");

  if (OutMessages)
    *OutMessages = strdup(MsgsOS.str().c_str());

  return Result;
}

LLVMBool LLVMVerifyFunction(LLVMValueRef Fn, LLVMVerifierFailureAction Action) {
  LLVMBool Result = verifyFunction(
      *unwrap<Function>(Fn), Action != LLVMReturnStatusAction ? &errs() : 0);

  if (Action == LLVMAbortProcessAction && Result)
    report_fatal_error("Broken function found, compilation aborted!");

  return Result;
}

void LLVMViewFunctionCFG(LLVMValueRef Fn) {
  Function *F = unwrap<Function>(Fn);
  F->viewCFG();
}

void LLVMViewFunctionCFGOnly(LLVMValueRef Fn) {
  Function *F = unwrap<Function>(Fn);
  F->viewCFGOnly();
}
