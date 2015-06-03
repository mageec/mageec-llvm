//===- PassManagerBuilder.cpp - Build Standard Pass -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the PassManagerBuilder class, which is used to set up a
// "standard" optimization sequence suitable for languages like C and C++.
//
//===----------------------------------------------------------------------===//


#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm-c/Transforms/PassManagerBuilder.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Verifier.h"
#include "llvm/PassManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Target/TargetLibraryInfo.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Vectorize.h"

using namespace llvm;

static cl::opt<bool>
RunLoopVectorization("vectorize-loops", cl::Hidden,
                     cl::desc("Run the Loop vectorization passes"));

static cl::opt<bool>
RunSLPVectorization("vectorize-slp", cl::Hidden,
                    cl::desc("Run the SLP vectorization passes"));

static cl::opt<bool>
RunBBVectorization("vectorize-slp-aggressive", cl::Hidden,
                    cl::desc("Run the BB vectorization passes"));

static cl::opt<bool>
UseGVNAfterVectorization("use-gvn-after-vectorization",
  cl::init(false), cl::Hidden,
  cl::desc("Run GVN instead of Early CSE after vectorization passes"));

static cl::opt<bool> UseNewSROA("use-new-sroa",
  cl::init(true), cl::Hidden,
  cl::desc("Enable the new, experimental SROA pass"));

static cl::opt<bool>
RunLoopRerolling("reroll-loops", cl::Hidden,
                 cl::desc("Run the loop rerolling pass"));

static cl::opt<bool> RunLoadCombine("combine-loads", cl::init(false),
                                    cl::Hidden,
                                    cl::desc("Run the load combining pass"));

static cl::opt<bool>
RunSLPAfterLoopVectorization("run-slp-after-loop-vectorization",
  cl::init(true), cl::Hidden,
  cl::desc("Run the SLP vectorizer (and BB vectorizer) after the Loop "
           "vectorizer instead of before"));

static cl::opt<bool> UseCFLAA("use-cfl-aa",
  cl::init(false), cl::Hidden,
  cl::desc("Enable the new, experimental CFL alias analysis"));

static cl::opt<bool>
EnableMLSM("mlsm", cl::init(true), cl::Hidden,
           cl::desc("Enable motion of merged load and store"));

PassManagerBuilder::PassManagerBuilder() {
    OptLevel = 2;
    SizeLevel = 0;
    LibraryInfo = nullptr;
    Inliner = nullptr;
    DisablePassPredicates = false;
    DisableTailCalls = false;
    DisableUnitAtATime = false;
    DisableUnrollLoops = false;
    BBVectorize = RunBBVectorization;
    SLPVectorize = RunSLPVectorization;
    LoopVectorize = RunLoopVectorization;
    RerollLoops = RunLoopRerolling;
    LoadCombine = RunLoadCombine;
    DisableGVNLoadPRE = false;
    VerifyInput = false;
    VerifyOutput = false;
    StripDebug = false;
    MergeFunctions = false;
}

PassManagerBuilder::~PassManagerBuilder() {
  delete LibraryInfo;
  delete Inliner;
}

/// Set of global extensions, automatically added as part of the standard set.
static ManagedStatic<SmallVector<std::pair<PassManagerBuilder::ExtensionPointTy,
   PassManagerBuilder::ExtensionFn>, 8> > GlobalExtensions;

void PassManagerBuilder::addGlobalExtension(
    PassManagerBuilder::ExtensionPointTy Ty,
    PassManagerBuilder::ExtensionFn Fn) {
  GlobalExtensions->push_back(std::make_pair(Ty, Fn));
}

void PassManagerBuilder::addExtension(ExtensionPointTy Ty, ExtensionFn Fn) {
  Extensions.push_back(std::make_pair(Ty, Fn));
}

void PassManagerBuilder::addExtensionsToPM(ExtensionPointTy ETy,
                                           PassManagerBase &PM) const {
  for (unsigned i = 0, e = GlobalExtensions->size(); i != e; ++i)
    if ((*GlobalExtensions)[i].first == ETy)
      (*GlobalExtensions)[i].second(*this, PM);
  for (unsigned i = 0, e = Extensions.size(); i != e; ++i)
    if (Extensions[i].first == ETy)
      Extensions[i].second(*this, PM);
}


void
PassManagerBuilder::addPass(PassManagerBase &PM, Pass *P)
{
  if (!DisablePassPredicates && P->mayHavePredicate()) {
    PM.add(P->getPredicateWrapperPass());
  }
  else {
    PM.add(P);
  }
}

void
PassManagerBuilder::addInitialAliasAnalysisPasses(PassManagerBase &PM) const {
  // Add TypeBasedAliasAnalysis before BasicAliasAnalysis so that
  // BasicAliasAnalysis wins if they disagree. This is intended to help
  // support "obvious" type-punning idioms.
  if (UseCFLAA)
    PM.add(createCFLAliasAnalysisPass());
  PM.add(createTypeBasedAliasAnalysisPass());
  PM.add(createScopedNoAliasAAPass());
  PM.add(createBasicAliasAnalysisPass());
}

void PassManagerBuilder::populateFunctionPassManager(FunctionPassManager &FPM) {
  addExtensionsToPM(EP_EarlyAsPossible, FPM);

  // Add LibraryInfo if we have some.
  if (LibraryInfo) addPass(FPM, new TargetLibraryInfo(*LibraryInfo));

  if (OptLevel == 0) return;

  addInitialAliasAnalysisPasses(FPM);

  addPass(FPM, createCFGSimplificationPass());
  if (UseNewSROA)
    addPass(FPM, createSROAPass());
  else
    addPass(FPM, createScalarReplAggregatesPass());
  addPass(FPM, createEarlyCSEPass());
  addPass(FPM, createLowerExpectIntrinsicPass());
}

void PassManagerBuilder::populateModulePassManager(PassManagerBase &MPM) {
  //MPM.add(createMageecFeatureExtractorPass());
  
  // If all optimizations are disabled, just run the always-inline pass.
  if (OptLevel == 0) {
    if (Inliner) {
      addPass(MPM, Inliner);
      Inliner = nullptr;
    }

    // FIXME: This is a HACK! The inliner pass above implicitly creates a CGSCC
    // pass manager, but we don't want to add extensions into that pass manager.
    // To prevent this we must insert a no-op module pass to reset the pass
    // manager to get the same behavior as EP_OptimizerLast in non-O0 builds.
    if (!GlobalExtensions->empty() || !Extensions.empty())
      addPass(MPM, createBarrierNoopPass());

    addExtensionsToPM(EP_EnabledOnOptLevel0, MPM);
    return;
  }

  // Add LibraryInfo if we have some.
  if (LibraryInfo) addPass(MPM, new TargetLibraryInfo(*LibraryInfo));

  addInitialAliasAnalysisPasses(MPM);

  if (!DisableUnitAtATime) {
    addExtensionsToPM(EP_ModuleOptimizerEarly, MPM);

    addPass(MPM, createIPSCCPPass());              // IP SCCP
    addPass(MPM, createGlobalOptimizerPass());     // Optimize out global vars

    addPass(MPM, createDeadArgEliminationPass());  // Dead argument elimination

    addPass(MPM, createInstructionCombiningPass());// Clean up after IPCP & DAE
    addExtensionsToPM(EP_Peephole, MPM);
    addPass(MPM, createCFGSimplificationPass());   // Clean up after IPCP & DAE
  }

  // Start of CallGraph SCC passes.
  if (!DisableUnitAtATime)
    addPass(MPM, createPruneEHPass());             // Remove dead EH info
  if (Inliner) {
    addPass(MPM, Inliner);
    Inliner = nullptr;
  }
  if (!DisableUnitAtATime)
    addPass(MPM, createFunctionAttrsPass());       // Set readonly/readnone attrs
  if (OptLevel > 2)
    addPass(MPM, createArgumentPromotionPass());   // Scalarize uninlined fn args

  // Start of function pass.
  // Break up aggregate allocas, using SSAUpdater.
  if (UseNewSROA)
    addPass(MPM, createSROAPass(/*RequiresDomTree*/ false));
  else
    addPass(MPM, createScalarReplAggregatesPass(-1, false));
  addPass(MPM, createEarlyCSEPass());              // Catch trivial redundancies
  addPass(MPM, createJumpThreadingPass());         // Thread jumps.
  addPass(MPM, createCorrelatedValuePropagationPass()); // Propagate conditionals
  addPass(MPM, createCFGSimplificationPass());     // Merge & remove BBs
  addPass(MPM, createInstructionCombiningPass());  // Combine silly seq's
  addExtensionsToPM(EP_Peephole, MPM);

  if (!DisableTailCalls)
    addPass(MPM, createTailCallEliminationPass()); // Eliminate tail calls
  addPass(MPM, createCFGSimplificationPass());     // Merge & remove BBs
  addPass(MPM, createReassociatePass());           // Reassociate expressions
  addPass(MPM, createLoopRotatePass());            // Rotate Loop
  addPass(MPM, createLICMPass());                  // Hoist loop invariants
  addPass(MPM, createLoopUnswitchPass(SizeLevel || OptLevel < 3));
  addPass(MPM, createInstructionCombiningPass());
  addPass(MPM, createIndVarSimplifyPass());        // Canonicalize indvars
  addPass(MPM, createLoopIdiomPass());             // Recognize idioms like memset.
  addPass(MPM, createLoopDeletionPass());          // Delete dead loops

  if (!DisableUnrollLoops)
    addPass(MPM, createSimpleLoopUnrollPass());    // Unroll small loops
  addExtensionsToPM(EP_LoopOptimizerEnd, MPM);

  if (OptLevel > 1) {
    if (EnableMLSM)
      addPass(MPM, createMergedLoadStoreMotionPass()); // Merge ld/st in diamonds
    addPass(MPM, createGVNPass(DisableGVNLoadPRE));  // Remove redundancies
  }
  addPass(MPM, createMemCpyOptPass());             // Remove memcpy / form memset
  addPass(MPM, createSCCPPass());                  // Constant prop with SCCP

  // Run instcombine after redundancy elimination to exploit opportunities
  // opened up by them.
  addPass(MPM, createInstructionCombiningPass());
  addExtensionsToPM(EP_Peephole, MPM);
  addPass(MPM, createJumpThreadingPass());         // Thread jumps
  addPass(MPM, createCorrelatedValuePropagationPass());
  addPass(MPM, createDeadStoreEliminationPass());  // Delete dead stores

  addExtensionsToPM(EP_ScalarOptimizerLate, MPM);

  if (RerollLoops)
    addPass(MPM, createLoopRerollPass());
  if (!RunSLPAfterLoopVectorization) {
    if (SLPVectorize)
      addPass(MPM, createSLPVectorizerPass());   // Vectorize parallel scalar chains.

    if (BBVectorize) {
      addPass(MPM, createBBVectorizePass());
      addPass(MPM, createInstructionCombiningPass());
      addExtensionsToPM(EP_Peephole, MPM);
      if (OptLevel > 1 && UseGVNAfterVectorization)
        addPass(MPM, createGVNPass(DisableGVNLoadPRE)); // Remove redundancies
      else
        addPass(MPM, createEarlyCSEPass());      // Catch trivial redundancies

      // BBVectorize may have significantly shortened a loop body; unroll again.
      if (!DisableUnrollLoops)
        addPass(MPM, createLoopUnrollPass());
    }
  }

  if (LoadCombine)
    addPass(MPM, createLoadCombinePass());

  addPass(MPM, createAggressiveDCEPass());         // Delete dead instructions
  addPass(MPM, createCFGSimplificationPass()); // Merge & remove BBs
  addPass(MPM, createInstructionCombiningPass());  // Clean up after everything.
  addExtensionsToPM(EP_Peephole, MPM);

  // FIXME: This is a HACK! The inliner pass above implicitly creates a CGSCC
  // pass manager that we are specifically trying to avoid. To prevent this
  // we must insert a no-op module pass to reset the pass manager.
  addPass(MPM, createBarrierNoopPass());
  addPass(MPM, createLoopVectorizePass(DisableUnrollLoops, LoopVectorize));
  // FIXME: Because of #pragma vectorize enable, the passes below are always
  // inserted in the pipeline, even when the vectorizer doesn't run (ex. when
  // on -O1 and no #pragma is found). Would be good to have these two passes
  // as function calls, so that we can only pass them when the vectorizer
  // changed the code.
  addPass(MPM, createInstructionCombiningPass());

  if (RunSLPAfterLoopVectorization) {
    if (SLPVectorize)
      addPass(MPM, createSLPVectorizerPass());   // Vectorize parallel scalar chains.

    if (BBVectorize) {
      addPass(MPM, createBBVectorizePass());
      addPass(MPM, createInstructionCombiningPass());
      addExtensionsToPM(EP_Peephole, MPM);
      if (OptLevel > 1 && UseGVNAfterVectorization)
        addPass(MPM, createGVNPass(DisableGVNLoadPRE)); // Remove redundancies
      else
        addPass(MPM, createEarlyCSEPass());      // Catch trivial redundancies

      // BBVectorize may have significantly shortened a loop body; unroll again.
      if (!DisableUnrollLoops)
        addPass(MPM, createLoopUnrollPass());
    }
  }

  addExtensionsToPM(EP_Peephole, MPM);
  addPass(MPM, createCFGSimplificationPass());

  if (!DisableUnrollLoops)
    addPass(MPM, createLoopUnrollPass());    // Unroll small loops

  // After vectorization and unrolling, assume intrinsics may tell us more
  // about pointer alignments.
  addPass(MPM, createAlignmentFromAssumptionsPass());

  if (!DisableUnitAtATime) {
    // FIXME: We shouldn't bother with this anymore.
    addPass(MPM, createStripDeadPrototypesPass()); // Get rid of dead prototypes

    // GlobalOpt already deletes dead functions and globals, at -O2 try a
    // late pass of GlobalDCE.  It is capable of deleting dead cycles.
    if (OptLevel > 1) {
      addPass(MPM, createGlobalDCEPass());         // Remove dead fns and globals.
      addPass(MPM, createConstantMergePass());     // Merge dup global constants
    }
  }

  if (MergeFunctions)
    addPass(MPM, createMergeFunctionsPass());

  addExtensionsToPM(EP_OptimizerLast, MPM);
}

void PassManagerBuilder::addLTOOptimizationPasses(PassManagerBase &PM) {
  // Provide AliasAnalysis services for optimizations.
  addInitialAliasAnalysisPasses(PM);

  // Propagate constants at call sites into the functions they call.  This
  // opens opportunities for globalopt (and inlining) by substituting function
  // pointers passed as arguments to direct uses of functions.
  addPass(PM, createIPSCCPPass());

  // Now that we internalized some globals, see if we can hack on them!
  addPass(PM, createGlobalOptimizerPass());

  // Linking modules together can lead to duplicated global constants, only
  // keep one copy of each constant.
  addPass(PM, createConstantMergePass());

  // Remove unused arguments from functions.
  addPass(PM, createDeadArgEliminationPass());

  // Reduce the code after globalopt and ipsccp.  Both can open up significant
  // simplification opportunities, and both can propagate functions through
  // function pointers.  When this happens, we often have to resolve varargs
  // calls, etc, so let instcombine do this.
  addPass(PM, createInstructionCombiningPass());
  addExtensionsToPM(EP_Peephole, PM);

  // Inline small functions
  bool RunInliner = Inliner;
  if (RunInliner) {
    addPass(PM, Inliner);
    Inliner = nullptr;
  }

  addPass(PM, createPruneEHPass());   // Remove dead EH info.

  // Optimize globals again if we ran the inliner.
  if (RunInliner)
    addPass(PM, createGlobalOptimizerPass());
  addPass(PM, createGlobalDCEPass()); // Remove dead functions.

  // If we didn't decide to inline a function, check to see if we can
  // transform it to pass arguments by value instead of by reference.
  addPass(PM, createArgumentPromotionPass());

  // The IPO passes may leave cruft around.  Clean up after them.
  addPass(PM, createInstructionCombiningPass());
  addExtensionsToPM(EP_Peephole, PM);
  addPass(PM, createJumpThreadingPass());

  // Break up allocas
  if (UseNewSROA)
    addPass(PM, createSROAPass());
  else
    addPass(PM, createScalarReplAggregatesPass());

  // Run a few AA driven optimizations here and now, to cleanup the code.
  addPass(PM, createFunctionAttrsPass()); // Add nocapture.
  addPass(PM, createGlobalsModRefPass()); // IP alias analysis.

  addPass(PM, createLICMPass());                 // Hoist loop invariants.
  if (EnableMLSM)
    addPass(PM, createMergedLoadStoreMotionPass()); // Merge ld/st in diamonds.
  addPass(PM, createGVNPass(DisableGVNLoadPRE)); // Remove redundancies.
  addPass(PM, createMemCpyOptPass());            // Remove dead memcpys.

  // Nuke dead stores.
  addPass(PM, createDeadStoreEliminationPass());

  // More loops are countable; try to optimize them.
  addPass(PM, createIndVarSimplifyPass());
  addPass(PM, createLoopDeletionPass());
  addPass(PM, createLoopVectorizePass(true, true));

  // More scalar chains could be vectorized due to more alias information
  addPass(PM, createSLPVectorizerPass()); // Vectorize parallel scalar chains.

  // After vectorization, assume intrinsics may tell us more about pointer
  // alignments.
  addPass(PM, createAlignmentFromAssumptionsPass());

  if (LoadCombine)
    addPass(PM, createLoadCombinePass());

  // Cleanup and simplify the code after the scalar optimizations.
  addPass(PM, createInstructionCombiningPass());
  addExtensionsToPM(EP_Peephole, PM);

  addPass(PM, createJumpThreadingPass());

  // Delete basic blocks, which optimization passes may have killed.
  addPass(PM, createCFGSimplificationPass());

  // Now that we have optimized the program, discard unreachable functions.
  addPass(PM, createGlobalDCEPass());

  // FIXME: this is profitable (for compiler time) to do at -O0 too, but
  // currently it damages debug info.
  if (MergeFunctions)
    addPass(PM, createMergeFunctionsPass());
}

void PassManagerBuilder::populateLTOPassManager(PassManagerBase &PM,
                                                TargetMachine *TM) {
  if (TM) {
    addPass(PM, new DataLayoutPass());
    TM->addAnalysisPasses(PM);
  }

  if (LibraryInfo)
    addPass(PM, new TargetLibraryInfo(*LibraryInfo));

  if (VerifyInput)
    addPass(PM, createVerifierPass());

  if (StripDebug)
    addPass(PM, createStripSymbolsPass(true));

  if (VerifyInput)
    addPass(PM, createDebugInfoVerifierPass());

  if (OptLevel != 0)
    addLTOOptimizationPasses(PM);

  if (VerifyOutput) {
    addPass(PM, createVerifierPass());
    addPass(PM, createDebugInfoVerifierPass());
  }
}

inline PassManagerBuilder *unwrap(LLVMPassManagerBuilderRef P) {
    return reinterpret_cast<PassManagerBuilder*>(P);
}

inline LLVMPassManagerBuilderRef wrap(PassManagerBuilder *P) {
  return reinterpret_cast<LLVMPassManagerBuilderRef>(P);
}

LLVMPassManagerBuilderRef LLVMPassManagerBuilderCreate() {
  PassManagerBuilder *PMB = new PassManagerBuilder();
  return wrap(PMB);
}

void LLVMPassManagerBuilderDispose(LLVMPassManagerBuilderRef PMB) {
  PassManagerBuilder *Builder = unwrap(PMB);
  delete Builder;
}

void
LLVMPassManagerBuilderSetOptLevel(LLVMPassManagerBuilderRef PMB,
                                  unsigned OptLevel) {
  PassManagerBuilder *Builder = unwrap(PMB);
  Builder->OptLevel = OptLevel;
}

void
LLVMPassManagerBuilderSetSizeLevel(LLVMPassManagerBuilderRef PMB,
                                   unsigned SizeLevel) {
  PassManagerBuilder *Builder = unwrap(PMB);
  Builder->SizeLevel = SizeLevel;
}

void
LLVMPassManagerBuilderSetDisableUnitAtATime(LLVMPassManagerBuilderRef PMB,
                                            LLVMBool Value) {
  PassManagerBuilder *Builder = unwrap(PMB);
  Builder->DisableUnitAtATime = Value;
}

void
LLVMPassManagerBuilderSetDisableUnrollLoops(LLVMPassManagerBuilderRef PMB,
                                            LLVMBool Value) {
  PassManagerBuilder *Builder = unwrap(PMB);
  Builder->DisableUnrollLoops = Value;
}

void
LLVMPassManagerBuilderSetDisableSimplifyLibCalls(LLVMPassManagerBuilderRef PMB,
                                                 LLVMBool Value) {
  // NOTE: The simplify-libcalls pass has been removed.
}

void
LLVMPassManagerBuilderUseInlinerWithThreshold(LLVMPassManagerBuilderRef PMB,
                                              unsigned Threshold) {
  PassManagerBuilder *Builder = unwrap(PMB);
  Builder->Inliner = createFunctionInliningPass(Threshold);
}

void
LLVMPassManagerBuilderPopulateFunctionPassManager(LLVMPassManagerBuilderRef PMB,
                                                  LLVMPassManagerRef PM) {
  PassManagerBuilder *Builder = unwrap(PMB);
  FunctionPassManager *FPM = unwrap<FunctionPassManager>(PM);
  Builder->populateFunctionPassManager(*FPM);
}

void
LLVMPassManagerBuilderPopulateModulePassManager(LLVMPassManagerBuilderRef PMB,
                                                LLVMPassManagerRef PM) {
  PassManagerBuilder *Builder = unwrap(PMB);
  PassManagerBase *MPM = unwrap(PM);
  Builder->populateModulePassManager(*MPM);
}

void LLVMPassManagerBuilderPopulateLTOPassManager(LLVMPassManagerBuilderRef PMB,
                                                  LLVMPassManagerRef PM,
                                                  LLVMBool Internalize,
                                                  LLVMBool RunInliner) {
  PassManagerBuilder *Builder = unwrap(PMB);
  PassManagerBase *LPM = unwrap(PM);

  // A small backwards compatibility hack. populateLTOPassManager used to take
  // an RunInliner option.
  if (RunInliner && !Builder->Inliner)
    Builder->Inliner = createFunctionInliningPass();

  Builder->populateLTOPassManager(*LPM);
}
