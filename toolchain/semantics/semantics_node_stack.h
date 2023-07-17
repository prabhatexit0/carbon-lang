// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_TOOLCHAIN_SEMANTICS_SEMANTICS_NODE_STACK_H_
#define CARBON_TOOLCHAIN_SEMANTICS_SEMANTICS_NODE_STACK_H_

#include <type_traits>

#include "common/vlog.h"
#include "llvm/ADT/SmallVector.h"
#include "toolchain/parser/parse_node_kind.h"
#include "toolchain/parser/parse_tree.h"
#include "toolchain/semantics/semantics_node.h"

namespace Carbon {

// Wraps the stack of nodes for SemanticsParseTreeHandler.
//
// All pushes and pops will be vlogged.
//
// Pop APIs will run basic verification:
//
// - If receiving a pop_parse_kind, verify that the parse_node being popped is
//   of pop_parse_kind.
// - Validates presence of node_id based on whether it's a solo
//   parse_node.
//
// These should be assumed API constraints unless otherwise mentioned on a
// method. The main exception is PopAndIgnore, which doesn't do verification.
class SemanticsNodeStack {
 public:
  explicit SemanticsNodeStack(const ParseTree& parse_tree,
                              llvm::raw_ostream* vlog_stream)
      : parse_tree_(&parse_tree), vlog_stream_(vlog_stream) {}

  // Pushes a solo parse tree node onto the stack. Used when there is no
  // IR generated by the node.
  auto Push(ParseTree::Node parse_node) -> void {
    CARBON_CHECK(ParseNodeKindToIdKind(parse_tree_->node_kind(parse_node)) ==
                 IdKind::SoloParseNode)
        << "Parse kind expects an Id: " << parse_tree_->node_kind(parse_node);
    CARBON_VLOG() << "Node Push " << stack_.size() << ": "
                  << parse_tree_->node_kind(parse_node) << " -> <none>\n";
    CARBON_CHECK(stack_.size() < (1 << 20))
        << "Excessive stack size: likely infinite loop";
    stack_.push_back(Entry(parse_node, SemanticsNodeId::Invalid));
  }

  // Pushes a parse tree node onto the stack with an ID.
  template <typename IdT>
  auto Push(ParseTree::Node parse_node, IdT id) -> void {
    CARBON_CHECK(ParseNodeKindToIdKind(parse_tree_->node_kind(parse_node)) ==
                 IdTypeToIdKind<IdT>())
        << "Parse kind expected a different IdT: "
        << parse_tree_->node_kind(parse_node) << " -> " << id << "\n";
    CARBON_CHECK(id.is_valid()) << "Push called with invalid id: "
                                << parse_tree_->node_kind(parse_node);
    CARBON_VLOG() << "Node Push " << stack_.size() << ": "
                  << parse_tree_->node_kind(parse_node) << " -> " << id << "\n";
    CARBON_CHECK(stack_.size() < (1 << 20))
        << "Excessive stack size: likely infinite loop";
    stack_.push_back(Entry(parse_node, id));
  }

  // Pops the top of the stack without any verification.
  auto PopAndIgnore() -> void { PopEntry<SemanticsNodeId>(); }

  // Pops the top of the stack and returns the parse_node.
  template <ParseNodeKind::RawEnumType RequiredParseKind>
  auto PopForSoloParseNode() -> ParseTree::Node {
    Entry back = PopEntry<SemanticsNodeId>();
    RequireIdKind(ParseNodeKind::Create(RequiredParseKind),
                  IdKind::SoloParseNode);
    RequireParseKind<RequiredParseKind>(back.parse_node);
    return back.parse_node;
  }

  // Pops the top of the stack.
  template <ParseNodeKind::RawEnumType RequiredParseKind>
  auto PopAndDiscardSoloParseNode() -> void {
    PopForSoloParseNode<RequiredParseKind>();
  }

  // Pops the top of the stack and returns the parse_node and the ID.
  auto PopExpressionWithParseNode()
      -> std::pair<ParseTree::Node, SemanticsNodeId> {
    return PopWithParseNode<SemanticsNodeId>();
  }

  // Pops the top of the stack and returns the parse_node and the ID.
  template <ParseNodeKind::RawEnumType RequiredParseKind>
  auto PopWithParseNode() -> auto {
    constexpr IdKind RequiredIdKind =
        ParseNodeKindToIdKind(ParseNodeKind::Create(RequiredParseKind));
    if constexpr (RequiredIdKind == IdKind::SemanticsNodeId) {
      auto back = PopWithParseNode<SemanticsNodeId>();
      RequireParseKind<RequiredParseKind>(back.first);
      return back;
    }
    if constexpr (RequiredIdKind == IdKind::SemanticsNodeBlockId) {
      auto back = PopWithParseNode<SemanticsNodeBlockId>();
      RequireParseKind<RequiredParseKind>(back.first);
      return back;
    }
    if constexpr (RequiredIdKind == IdKind::SemanticsFunctionId) {
      auto back = PopWithParseNode<SemanticsFunctionId>();
      RequireParseKind<RequiredParseKind>(back.first);
      return back;
    }
    if constexpr (RequiredIdKind == IdKind::SemanticsStringId) {
      auto back = PopWithParseNode<SemanticsStringId>();
      RequireParseKind<RequiredParseKind>(back.first);
      return back;
    }
    if constexpr (RequiredIdKind == IdKind::SemanticsTypeId) {
      auto back = PopWithParseNode<SemanticsTypeId>();
      RequireParseKind<RequiredParseKind>(back.first);
      return back;
    }
    CARBON_FATAL() << "Unpoppable IdKind for parse kind: "
                   << ParseNodeKind::Create(RequiredParseKind)
                   << "; see value in ParseNodeKindToIdKind";
  }

  // Pops an expression from the top of the stack and returns the ID.
  // Expressions map multiple ParseNodeKinds to SemanticsNodeId always.
  auto PopExpression() -> SemanticsNodeId {
    return PopExpressionWithParseNode().second;
  }

  // Pops the top of the stack and returns the ID.
  template <ParseNodeKind::RawEnumType RequiredParseKind>
  auto Pop() -> auto {
    return PopWithParseNode<RequiredParseKind>().second;
  }

  // Peeks at the parse_node of the top of the stack.
  auto PeekParseNode() -> ParseTree::Node { return stack_.back().parse_node; }

  // Peeks at the ID of the top of the stack.
  template <ParseNodeKind::RawEnumType RequiredParseKind>
  auto Peek() -> auto {
    Entry back = stack_.back();
    RequireParseKind<RequiredParseKind>(back.parse_node);
    constexpr IdKind RequiredIdKind =
        ParseNodeKindToIdKind(ParseNodeKind::Create(RequiredParseKind));
    if constexpr (RequiredIdKind == IdKind::SemanticsNodeId) {
      return back.id<SemanticsNodeId>();
    }
    if constexpr (RequiredIdKind == IdKind::SemanticsNodeBlockId) {
      return back.id<SemanticsNodeBlockId>();
    }
    if constexpr (RequiredIdKind == IdKind::SemanticsFunctionId) {
      return back.id<SemanticsFunctionId>();
    }
    if constexpr (RequiredIdKind == IdKind::SemanticsStringId) {
      return back.id<SemanticsStringId>();
    }
    if constexpr (RequiredIdKind == IdKind::SemanticsTypeId) {
      return back.id<SemanticsTypeId>();
    }
    CARBON_FATAL() << "Unpeekable IdKind for parse kind: "
                   << ParseNodeKind::Create(RequiredParseKind)
                   << "; see value in ParseNodeKindToIdKind";
  }

  // Prints the stack for a stack dump.
  auto PrintForStackDump(llvm::raw_ostream& output) const -> void;

  auto empty() const -> bool { return stack_.empty(); }
  auto size() const -> size_t { return stack_.size(); }

 private:
  // Possible associated ID types.
  enum class IdKind {
    SemanticsNodeId,
    SemanticsNodeBlockId,
    SemanticsFunctionId,
    SemanticsStringId,
    SemanticsTypeId,
    // No associated ID type.
    SoloParseNode,
    // Not expected in the node stack.
    Unused,
  };

  // An entry in stack_.
  struct Entry {
    explicit Entry(ParseTree::Node parse_node, SemanticsNodeId node_id)
        : parse_node(parse_node), node_id(node_id) {}
    explicit Entry(ParseTree::Node parse_node,
                   SemanticsNodeBlockId node_block_id)
        : parse_node(parse_node), node_block_id(node_block_id) {}
    explicit Entry(ParseTree::Node parse_node, SemanticsFunctionId function_id)
        : parse_node(parse_node), function_id(function_id) {}
    explicit Entry(ParseTree::Node parse_node, SemanticsStringId name_id)
        : parse_node(parse_node), name_id(name_id) {}
    explicit Entry(ParseTree::Node parse_node, SemanticsTypeId type_id)
        : parse_node(parse_node), type_id(type_id) {}

    // Returns the appropriate ID basaed on type.
    template <typename T>
    auto id() -> T& {
      if constexpr (std::is_same<T, SemanticsNodeId>()) {
        return node_id;
      }
      if constexpr (std::is_same<T, SemanticsNodeBlockId>()) {
        return node_block_id;
      }
      if constexpr (std::is_same<T, SemanticsFunctionId>()) {
        return function_id;
      }
      if constexpr (std::is_same<T, SemanticsStringId>()) {
        return name_id;
      }
      if constexpr (std::is_same<T, SemanticsTypeId>()) {
        return type_id;
      }
    }

    // The node associated with the stack entry.
    ParseTree::Node parse_node;

    // The entries will evaluate as invalid if and only if they're a solo
    // parse_node. Invalid is used instead of optional to save space.
    //
    // A discriminator isn't needed because the caller can determine which field
    // is used based on the ParseNodeKind.
    union {
      SemanticsNodeId node_id;
      SemanticsNodeBlockId node_block_id;
      SemanticsFunctionId function_id;
      SemanticsStringId name_id;
      SemanticsTypeId type_id;
    };
  };
  static_assert(sizeof(Entry) == 8, "Unexpected Entry size");

  // Translate a parse node kind to the enum ID kind it should always provide.
  static constexpr auto ParseNodeKindToIdKind(ParseNodeKind kind) -> IdKind {
    switch (kind) {
      case Carbon::ParseNodeKind::CallExpression:
      case Carbon::ParseNodeKind::CallExpressionStart:
      case Carbon::ParseNodeKind::IfExpressionElse:
      case Carbon::ParseNodeKind::InfixOperator:
      case Carbon::ParseNodeKind::Literal:
      case Carbon::ParseNodeKind::MemberAccessExpression:
      case Carbon::ParseNodeKind::NameExpression:
      case Carbon::ParseNodeKind::ParenExpression:
      case Carbon::ParseNodeKind::PatternBinding:
      case Carbon::ParseNodeKind::PrefixOperator:
      case Carbon::ParseNodeKind::ShortCircuitOperand:
      case Carbon::ParseNodeKind::StructFieldValue:
      case Carbon::ParseNodeKind::StructLiteral:
      case Carbon::ParseNodeKind::StructTypeLiteral:
        return IdKind::SemanticsNodeId;
      case Carbon::ParseNodeKind::IfExpressionThen:
      case Carbon::ParseNodeKind::IfStatementElse:
      case Carbon::ParseNodeKind::ParameterList:
        return IdKind::SemanticsNodeBlockId;
      case Carbon::ParseNodeKind::FunctionDefinitionStart:
        return IdKind::SemanticsFunctionId;
      case Carbon::ParseNodeKind::Name:
        return IdKind::SemanticsStringId;
      case Carbon::ParseNodeKind::ReturnType:
        return IdKind::SemanticsTypeId;
      case Carbon::ParseNodeKind::CodeBlockStart:
      case Carbon::ParseNodeKind::FunctionIntroducer:
      case Carbon::ParseNodeKind::IfCondition:
      case Carbon::ParseNodeKind::IfExpressionIf:
      case Carbon::ParseNodeKind::ParameterListStart:
      case Carbon::ParseNodeKind::ParenExpressionOrTupleLiteralStart:
      case Carbon::ParseNodeKind::QualifiedDeclaration:
      case Carbon::ParseNodeKind::ReturnStatementStart:
      case Carbon::ParseNodeKind::StructFieldType:
      case Carbon::ParseNodeKind::StructLiteralOrStructTypeLiteralStart:
      case Carbon::ParseNodeKind::VariableInitializer:
      case Carbon::ParseNodeKind::VariableIntroducer:
        return IdKind::SoloParseNode;
      default:
        return IdKind::Unused;
    }
  }

  // Translates an ID type to the enum ID kind for comparison with
  // ParseNodeKindToIdKind.
  template <typename IdT>
  static constexpr auto IdTypeToIdKind() -> IdKind {
    if constexpr (std::is_same_v<IdT, SemanticsNodeId>) {
      return IdKind::SemanticsNodeId;
    }
    if constexpr (std::is_same_v<IdT, SemanticsNodeBlockId>) {
      return IdKind::SemanticsNodeBlockId;
    }
    if constexpr (std::is_same_v<IdT, SemanticsFunctionId>) {
      return IdKind::SemanticsFunctionId;
    }
    if constexpr (std::is_same_v<IdT, SemanticsStringId>) {
      return IdKind::SemanticsStringId;
    }
    if constexpr (std::is_same_v<IdT, SemanticsTypeId>) {
      return IdKind::SemanticsTypeId;
    }
  }

  // Pops an entry.
  template <typename IdT>
  auto PopEntry() -> Entry {
    Entry back = stack_.pop_back_val();
    CARBON_VLOG() << "Node Pop " << stack_.size() << ": "
                  << parse_tree_->node_kind(back.parse_node) << " -> "
                  << back.id<IdT>() << "\n";
    return back;
  }

  // Pops the top of the stack and returns the parse_node and the ID.
  template <typename IdT>
  auto PopWithParseNode() -> std::pair<ParseTree::Node, IdT> {
    Entry back = PopEntry<IdT>();
    RequireIdKind(parse_tree_->node_kind(back.parse_node),
                  IdTypeToIdKind<IdT>());
    return {back.parse_node, back.id<IdT>()};
  }

  // Require a ParseNodeKind be mapped to a particular IdKind.
  auto RequireIdKind(ParseNodeKind parse_kind, IdKind id_kind) -> void {
    // TODO: Name can be popped as a node_id by declaration name handling. Will
    // refactor to remove this quirk.
    if (parse_kind == ParseNodeKind::Name &&
        id_kind == IdKind::SemanticsNodeId) {
      return;
    }
    CARBON_CHECK(ParseNodeKindToIdKind(parse_kind) == id_kind)
        << "Unexpected IdKind mapping for " << parse_kind;
  }

  // Require an entry to have the given ParseNodeKind.
  template <ParseNodeKind::RawEnumType RequiredParseKind>
  auto RequireParseKind(ParseTree::Node parse_node) -> void {
    auto actual_kind = parse_tree_->node_kind(parse_node);
    CARBON_CHECK(RequiredParseKind == actual_kind)
        << "Expected " << ParseNodeKind::Create(RequiredParseKind) << ", found "
        << actual_kind;
  }

  // The file's parse tree.
  const ParseTree* parse_tree_;

  // Whether to print verbose output.
  llvm::raw_ostream* vlog_stream_;

  // The actual stack.
  // PushEntry and PopEntry control modification in order to centralize
  // vlogging.
  llvm::SmallVector<Entry> stack_;
};

}  // namespace Carbon

#endif  // CARBON_TOOLCHAIN_SEMANTICS_SEMANTICS_NODE_STACK_H_
