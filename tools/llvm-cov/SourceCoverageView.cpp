//===- SourceCoverageView.cpp - Code coverage view for source code --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class implements rendering for code coverage of source code.
//
//===----------------------------------------------------------------------===//

#include "SourceCoverageView.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/LineIterator.h"

using namespace llvm;

void SourceCoverageView::renderLine(raw_ostream &OS, StringRef Line,
                                    ArrayRef<HighlightRange> Ranges) {
  if (Ranges.empty()) {
    OS << Line << "\n";
    return;
  }
  if (Line.empty())
    Line = " ";

  unsigned PrevColumnStart = 0;
  unsigned Start = 1;
  for (const auto &Range : Ranges) {
    if (PrevColumnStart == Range.ColumnStart)
      continue;

    // Show the unhighlighted part
    unsigned ColumnStart = PrevColumnStart = Range.ColumnStart;
    OS << Line.substr(Start - 1, ColumnStart - Start);

    // Show the highlighted part
    auto Color = Range.Kind == HighlightRange::NotCovered ? raw_ostream::RED
                                                          : raw_ostream::CYAN;
    OS.changeColor(Color, false, true);
    unsigned ColumnEnd = std::min(Range.ColumnEnd, (unsigned)Line.size() + 1);
    OS << Line.substr(ColumnStart - 1, ColumnEnd - ColumnStart);
    Start = ColumnEnd;
    OS.resetColor();
  }

  // Show the rest of the line
  OS << Line.substr(Start - 1, Line.size() - Start + 1);
  OS << "\n";

  if (Options.Debug) {
    for (const auto &Range : Ranges) {
      errs() << "Highlighted line " << Range.Line << ", " << Range.ColumnStart
             << " -> ";
      if (Range.ColumnEnd == std::numeric_limits<unsigned>::max()) {
        errs() << "?\n";
      } else {
        errs() << Range.ColumnEnd << "\n";
      }
    }
  }
}

void SourceCoverageView::renderIndent(raw_ostream &OS, unsigned Level) {
  for (unsigned I = 0; I < Level; ++I)
    OS << "  |";
}

void SourceCoverageView::renderViewDivider(unsigned Level, unsigned Length,
                                           raw_ostream &OS) {
  assert(Level != 0 && "Cannot render divider at top level");
  renderIndent(OS, Level - 1);
  OS.indent(2);
  for (unsigned I = 0; I < Length; ++I)
    OS << "-";
}

void
SourceCoverageView::renderLineCoverageColumn(raw_ostream &OS,
                                             const LineCoverageInfo &Line) {
  if (!Line.isMapped()) {
    OS.indent(LineCoverageColumnWidth) << '|';
    return;
  }
  SmallString<32> Buffer;
  raw_svector_ostream BufferOS(Buffer);
  BufferOS << Line.ExecutionCount;
  auto Str = BufferOS.str();
  // Trim
  Str = Str.substr(0, std::min(Str.size(), (size_t)LineCoverageColumnWidth));
  // Align to the right
  OS.indent(LineCoverageColumnWidth - Str.size());
  colored_ostream(OS, raw_ostream::MAGENTA,
                  Line.hasMultipleRegions() && Options.Colors)
      << Str;
  OS << '|';
}

void SourceCoverageView::renderLineNumberColumn(raw_ostream &OS,
                                                unsigned LineNo) {
  SmallString<32> Buffer;
  raw_svector_ostream BufferOS(Buffer);
  BufferOS << LineNo;
  auto Str = BufferOS.str();
  // Trim and align to the right
  Str = Str.substr(0, std::min(Str.size(), (size_t)LineNumberColumnWidth));
  OS.indent(LineNumberColumnWidth - Str.size()) << Str << '|';
}

void SourceCoverageView::renderRegionMarkers(raw_ostream &OS,
                                             ArrayRef<RegionMarker> Regions) {
  SmallString<32> Buffer;
  raw_svector_ostream BufferOS(Buffer);

  unsigned PrevColumn = 1;
  for (const auto &Region : Regions) {
    // Skip to the new region
    if (Region.Column > PrevColumn)
      OS.indent(Region.Column - PrevColumn);
    PrevColumn = Region.Column + 1;
    BufferOS << Region.ExecutionCount;
    StringRef Str = BufferOS.str();
    // Trim the execution count
    Str = Str.substr(0, std::min(Str.size(), (size_t)7));
    PrevColumn += Str.size();
    OS << '^' << Str;
    Buffer.clear();
  }
  OS << "\n";

  if (Options.Debug) {
    for (const auto &Region : Regions) {
      errs() << "Marker at " << Region.Line << ":" << Region.Column << " = "
             << Region.ExecutionCount << "\n";
    }
  }
}

/// \brief Insert a new highlighting range into the line's highlighting ranges
/// Return line's new highlighting ranges in result.
static void insertExpansionHighlightRange(
    ArrayRef<SourceCoverageView::HighlightRange> Ranges,
    unsigned Line, unsigned StartCol, unsigned EndCol,
    SmallVectorImpl<SourceCoverageView::HighlightRange> &Result) {
  auto RangeToInsert = SourceCoverageView::HighlightRange(
      Line, StartCol, EndCol, SourceCoverageView::HighlightRange::Expanded);
  Result.clear();
  size_t I = 0;
  auto E = Ranges.size();
  for (; I < E; ++I) {
    if (RangeToInsert.ColumnStart < Ranges[I].ColumnEnd) {
      const auto &Range = Ranges[I];
      bool NextRangeContainsInserted = false;
      // If the next range starts before the inserted range, move the end of the
      // next range to the start of the inserted range.
      if (Range.ColumnStart < RangeToInsert.ColumnStart) {
        if (RangeToInsert.ColumnStart != Range.ColumnStart)
          Result.push_back(SourceCoverageView::HighlightRange(
              Range.Line, Range.ColumnStart, RangeToInsert.ColumnStart,
              Range.Kind));
        // If the next range also ends after the inserted range, keep this range
        // and create a new range that starts at the inserted range and ends
        // at the next range later.
        if (Range.ColumnEnd > RangeToInsert.ColumnEnd)
          NextRangeContainsInserted = true;
      }
      if (!NextRangeContainsInserted) {
        ++I;
        // Ignore ranges that are contained in inserted range
        while (I < E && RangeToInsert.contains(Ranges[I]))
          ++I;
      }
      break;
    }
    Result.push_back(Ranges[I]);
  }
  Result.push_back(RangeToInsert);
  // If the next range starts before the inserted range end, move the start
  // of the next range to the end of the inserted range.
  if (I < E && Ranges[I].ColumnStart < RangeToInsert.ColumnEnd) {
    const auto &Range = Ranges[I];
    if (RangeToInsert.ColumnEnd != Range.ColumnEnd)
      Result.push_back(SourceCoverageView::HighlightRange(
          Range.Line, RangeToInsert.ColumnEnd, Range.ColumnEnd, Range.Kind));
    ++I;
  }
  // Add the remaining ranges that are located after the inserted range
  for (; I < E; ++I)
    Result.push_back(Ranges[I]);
}

template <typename T>
ArrayRef<T> gatherLineItems(size_t &CurrentIdx, const std::vector<T> &Items,
                            unsigned LineNo) {
  auto PrevIdx = CurrentIdx;
  auto E = Items.size();
  while (CurrentIdx < E && Items[CurrentIdx].Line == LineNo)
    ++CurrentIdx;
  return ArrayRef<T>(Items.data() + PrevIdx, CurrentIdx - PrevIdx);
}

void SourceCoverageView::render(raw_ostream &OS, unsigned IndentLevel) {
  SmallVector<HighlightRange, 8> AdjustedLineHighlightRanges;
  size_t CurrentHighlightRange = 0;
  size_t CurrentRegionMarker = 0;

  line_iterator Lines(File);
  // Advance the line iterator to the first line.
  while (Lines.line_number() < LineOffset)
    ++Lines;

  // The width of the leading columns
  unsigned CombinedColumnWidth =
      (Options.ShowLineStats ? LineCoverageColumnWidth + 1 : 0) +
      (Options.ShowLineNumbers ? LineNumberColumnWidth + 1 : 0);
  // The width of the line that is used to divide between the view and the
  // subviews.
  unsigned DividerWidth = CombinedColumnWidth + 4;

  // We need the expansions and instantiations sorted so we can go through them
  // while we iterate lines.
  std::sort(ExpansionSubViews.begin(), ExpansionSubViews.end());
  std::sort(InstantiationSubViews.begin(), InstantiationSubViews.end());
  auto NextESV = ExpansionSubViews.begin();
  auto EndESV = ExpansionSubViews.end();
  auto NextISV = InstantiationSubViews.begin();
  auto EndISV = InstantiationSubViews.end();

  for (size_t I = 0, E = LineStats.size(); I < E; ++I) {
    unsigned LineNo = I + LineOffset;

    renderIndent(OS, IndentLevel);
    if (Options.ShowLineStats)
      renderLineCoverageColumn(OS, LineStats[I]);
    if (Options.ShowLineNumbers)
      renderLineNumberColumn(OS, LineNo);

    // Gather highlighting ranges.
    auto LineHighlightRanges =
        gatherLineItems(CurrentHighlightRange, HighlightRanges, LineNo);
    auto LineRanges = LineHighlightRanges;
    // Highlight the expansion range if there is an expansion subview on this
    // line.
    if (NextESV != EndESV && NextESV->getLine() == LineNo && Options.Colors) {
      insertExpansionHighlightRange(
          LineHighlightRanges, NextESV->getLine(), NextESV->getStartCol(),
          NextESV->getEndCol(), AdjustedLineHighlightRanges);
      LineRanges = AdjustedLineHighlightRanges;
    }

    // Display the source code for the current line.
    StringRef Line = *Lines;
    // Check if the line is empty, as line_iterator skips blank lines.
    if (LineNo < Lines.line_number())
      Line = "";
    else if (!Lines.is_at_eof())
      ++Lines;
    renderLine(OS, Line, LineRanges);

    // Show the region markers.
    bool ShowMarkers = !Options.ShowLineStatsOrRegionMarkers ||
                       LineStats[I].hasMultipleRegions();
    auto LineMarkers = gatherLineItems(CurrentRegionMarker, Markers, LineNo);
    if (ShowMarkers && !LineMarkers.empty()) {
      renderIndent(OS, IndentLevel);
      OS.indent(CombinedColumnWidth);
      renderRegionMarkers(OS, LineMarkers);
    }

    // Show the expansions and instantiations for this line.
    unsigned NestedIndent = IndentLevel + 1;
    bool RenderedSubView = false;
    for (; NextESV != EndESV && NextESV->getLine() == LineNo; ++NextESV) {
      renderViewDivider(NestedIndent, DividerWidth, OS);
      OS << "\n";
      if (RenderedSubView) {
        // Re-render the current line and highlight the expansion range for
        // this subview.
        insertExpansionHighlightRange(
            LineHighlightRanges, NextESV->getLine(), NextESV->getStartCol(),
            NextESV->getEndCol(), AdjustedLineHighlightRanges);
        renderIndent(OS, IndentLevel);
        OS.indent(CombinedColumnWidth + (IndentLevel == 0 ? 0 : 1));
        renderLine(OS, Line, AdjustedLineHighlightRanges);
        renderViewDivider(NestedIndent, DividerWidth, OS);
        OS << "\n";
      }
      // Render the child subview
      NextESV->View->render(OS, NestedIndent);
      RenderedSubView = true;
    }
    for (; NextISV != EndISV && NextISV->Line == LineNo; ++NextISV) {
      renderViewDivider(NestedIndent, DividerWidth, OS);
      OS << "\n";
      renderIndent(OS, NestedIndent);
      OS << ' ';
      Options.colored_ostream(OS, raw_ostream::CYAN) << NextISV->FunctionName
                                                     << ":";
      OS << "\n";
      NextISV->View->render(OS, NestedIndent);
      RenderedSubView = true;
    }
    if (RenderedSubView) {
      renderViewDivider(NestedIndent, DividerWidth, OS);
      OS << "\n";
    }
  }
}

void SourceCoverageView::setUpVisibleRange(SourceCoverageDataManager &Data) {
  auto CountedRegions = Data.getSourceRegions();
  if (!CountedRegions.size())
    return;

  unsigned Start = CountedRegions.front().LineStart, End = 0;
  for (const auto &CR : CountedRegions) {
    Start = std::min(Start, CR.LineStart);
    End = std::max(End, CR.LineEnd);
  }
  LineOffset = Start;
  LineStats.resize(End - Start + 1);
}

void
SourceCoverageView::createLineCoverageInfo(SourceCoverageDataManager &Data) {
  auto CountedRegions = Data.getSourceRegions();
  for (const auto &CR : CountedRegions) {
    if (CR.Kind == coverage::CounterMappingRegion::SkippedRegion) {
      // Reset the line stats for skipped regions.
      for (unsigned Line = CR.LineStart; Line <= CR.LineEnd;
           ++Line)
        LineStats[Line - LineOffset] = LineCoverageInfo();
      continue;
    }
    LineStats[CR.LineStart - LineOffset].addRegionStartCount(CR.ExecutionCount);
    for (unsigned Line = CR.LineStart + 1; Line <= CR.LineEnd; ++Line)
      LineStats[Line - LineOffset].addRegionCount(CR.ExecutionCount);
  }
}

void
SourceCoverageView::createHighlightRanges(SourceCoverageDataManager &Data) {
  auto CountedRegions = Data.getSourceRegions();
  std::vector<bool> AlreadyHighlighted;
  AlreadyHighlighted.resize(CountedRegions.size(), false);

  for (size_t I = 0, S = CountedRegions.size(); I < S; ++I) {
    const auto &CR = CountedRegions[I];
    if (CR.Kind == coverage::CounterMappingRegion::SkippedRegion ||
        CR.ExecutionCount != 0)
      continue;
    if (AlreadyHighlighted[I])
      continue;
    for (size_t J = 0; J < S; ++J) {
      if (CR.contains(CountedRegions[J])) {
        AlreadyHighlighted[J] = true;
      }
    }
    if (CR.LineStart == CR.LineEnd) {
      HighlightRanges.push_back(HighlightRange(
          CR.LineStart, CR.ColumnStart, CR.ColumnEnd));
      continue;
    }
    HighlightRanges.push_back(
        HighlightRange(CR.LineStart, CR.ColumnStart,
                       std::numeric_limits<unsigned>::max()));
    HighlightRanges.push_back(
        HighlightRange(CR.LineEnd, 1, CR.ColumnEnd));
    for (unsigned Line = CR.LineStart + 1; Line < CR.LineEnd;
         ++Line) {
      HighlightRanges.push_back(
          HighlightRange(Line, 1, std::numeric_limits<unsigned>::max()));
    }
  }

  std::sort(HighlightRanges.begin(), HighlightRanges.end());
}

void SourceCoverageView::createRegionMarkers(SourceCoverageDataManager &Data) {
  for (const auto &CR : Data.getSourceRegions())
    if (CR.Kind != coverage::CounterMappingRegion::SkippedRegion)
      Markers.push_back(
          RegionMarker(CR.LineStart, CR.ColumnStart, CR.ExecutionCount));
}

void SourceCoverageView::load(SourceCoverageDataManager &Data) {
  setUpVisibleRange(Data);
  if (Options.ShowLineStats)
    createLineCoverageInfo(Data);
  if (Options.Colors)
    createHighlightRanges(Data);
  if (Options.ShowRegionMarkers)
    createRegionMarkers(Data);
}
