// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

#ifndef CARBON_EXPLORER_INTERPRETER_INTERPRETER_H_
#define CARBON_EXPLORER_INTERPRETER_INTERPRETER_H_

#include <optional>
#include <utility>
#include <vector>

#include "action_stack.h"
#include "common/ostream.h"
#include "explorer/ast/ast.h"
#include "explorer/ast/declaration.h"
#include "explorer/ast/expression.h"
#include "explorer/ast/pattern.h"
#include "explorer/ast/value.h"
#include "explorer/common/trace_stream.h"
#include "explorer/interpreter/action.h"
#include "explorer/interpreter/heap.h"
#include "llvm/ADT/ArrayRef.h"

namespace Carbon {

// Constructs an ActionStack suitable for the specified phase.
inline auto MakeTodo(Phase phase, Nonnull<Heap*> heap,
                     Nonnull<TraceStream*> trace_stream) -> ActionStack {
  switch (phase) {
    case Phase::CompileTime:
      return ActionStack(trace_stream);
    case Phase::RunTime:
      return ActionStack(trace_stream, heap);
  }
}

// An Interpreter represents an instance of the Carbon abstract machine. It
// manages the state of the abstract machine, and executes the steps of Actions
// passed to it.
class Interpreter {
 public:
  // Constructs an Interpreter which allocates values on `arena`, and prints
  // traces if `trace` is true. `phase` indicates whether it executes at
  // compile time or run time.
  Interpreter(Phase phase, Nonnull<Arena*> arena,
              Nonnull<TraceStream*> trace_stream,
              Nonnull<llvm::raw_ostream*> print_stream)
      : arena_(arena),
        heap_(trace_stream, arena),
        todo_(MakeTodo(phase, &heap_, trace_stream)),
        trace_stream_(trace_stream),
        print_stream_(print_stream),
        phase_(phase) {}

  // Runs all the steps of `action`.
  // It's not safe to call `RunAllSteps()` or `result()` after an error.
  auto RunAllSteps(std::unique_ptr<Action> action) -> ErrorOr<Success>;

  // The result produced by the `action` argument of the most recent
  // RunAllSteps call. Cannot be called if `action` was an action that doesn't
  // produce results.
  auto result() const -> Nonnull<const Value*> { return todo_.result(); }

 private:
  auto Step() -> ErrorOr<Success>;

  // State transitions for expressions value generation.
  auto StepValueExp() -> ErrorOr<Success>;
  // State transitions for expressions.
  auto StepExp() -> ErrorOr<Success>;
  // State transitions for lvalues.
  auto StepLocation() -> ErrorOr<Success>;
  // State transitions for witnesses.
  auto StepWitness() -> ErrorOr<Success>;
  // State transition for statements.
  auto StepStmt() -> ErrorOr<Success>;
  // State transition for declarations.
  auto StepDeclaration() -> ErrorOr<Success>;
  // State transition for object destruction.
  auto StepCleanUp() -> ErrorOr<Success>;
  auto StepDestroy() -> ErrorOr<Success>;
  // State transition for type instantiation.
  auto StepInstantiateType() -> ErrorOr<Success>;

  auto CreateStruct(const std::vector<FieldInitializer>& fields,
                    const std::vector<Nonnull<const Value*>>& values)
      -> Nonnull<const Value*>;

  auto EvalPrim(Operator op, Nonnull<const Value*> static_type,
                const std::vector<Nonnull<const Value*>>& args,
                SourceLocation source_loc) -> ErrorOr<Nonnull<const Value*>>;

  // Returns the result of converting `value` to type `destination_type`.
  auto Convert(Nonnull<const Value*> value,
               Nonnull<const Value*> destination_type,
               SourceLocation source_loc) -> ErrorOr<Nonnull<const Value*>>;

  // Create a class value and its base class(es) from an init struct.
  auto ConvertStructToClass(Nonnull<const StructValue*> init,
                            Nonnull<const NominalClassType*> class_type,
                            SourceLocation source_loc)
      -> ErrorOr<Nonnull<const NominalClassValue*>>;

  // Evaluate an expression immediately, recursively, and return its result.
  //
  // TODO: Stop using this.
  auto EvalRecursively(std::unique_ptr<Action> action)
      -> ErrorOr<Nonnull<const Value*>>;

  // Evaluate an associated constant by evaluating its witness and looking
  // inside the impl for the corresponding value.
  //
  // TODO: This approach doesn't provide values that are known because they
  // appear in constraints:
  //
  //   interface Iface { let N:! i32; }
  //   fn PickType(N: i32) -> type { return i32; }
  //   fn F[T:! Iface where .N == 5](x: T) {
  //     var x: PickType(T.N) = 0;
  //   }
  //
  // ... will fail because we can't resolve T.N to 5 at compile time.
  auto EvalAssociatedConstant(Nonnull<const AssociatedConstant*> assoc,
                              SourceLocation source_loc)
      -> ErrorOr<Nonnull<const Value*>>;

  // Instantiate a type by replacing all type variables that occur inside the
  // type by the current values of those variables.
  //
  // For example, suppose T=i32 and U=bool. Then
  //     __Fn (Point(T)) -> Point(U)
  // becomes
  //     __Fn (Point(i32)) -> Point(bool)
  //
  // TODO: This should be an Action.
  auto InstantiateType(Nonnull<const Value*> type, SourceLocation source_loc)
      -> ErrorOr<Nonnull<const Value*>>;

  // Instantiate a set of bindings by replacing all type variables that occur
  // within it by the current values of those variables.
  auto InstantiateBindings(Nonnull<const Bindings*> bindings,
                           SourceLocation source_loc)
      -> ErrorOr<Nonnull<const Bindings*>>;

  // Instantiate a witness by replacing all type variables and impl binding
  // references that occur within it by the current values of those variables.
  auto InstantiateWitness(Nonnull<const Witness*> witness,
                          SourceLocation source_loc)
      -> ErrorOr<Nonnull<const Witness*>>;

  // Call the function `fun` with the given `arg` and the `witnesses`
  // for the function's impl bindings.
  auto CallFunction(const CallExpression& call, Nonnull<const Value*> fun,
                    Nonnull<const Value*> arg, ImplWitnessMap&& witnesses,
                    std::optional<AllocationId> location_received)
      -> ErrorOr<Success>;

  auto CallDestructor(Nonnull<const DestructorDeclaration*> fun,
                      Nonnull<const Value*> receiver) -> ErrorOr<Success>;

  auto phase() const -> Phase { return phase_; }

  Nonnull<Arena*> arena_;

  Heap heap_;
  ActionStack todo_;

  Nonnull<TraceStream*> trace_stream_;

  // The stream for the Print intrinsic.
  Nonnull<llvm::raw_ostream*> print_stream_;

  Phase phase_;

  // The number of steps taken by the interpreter. Used for infinite loop
  // detection.
  int64_t steps_taken_ = 0;
};

// Interprets the program defined by `ast`, allocating values on `arena` and
// printing traces if `trace` is true.
auto InterpProgram(const AST& ast, Nonnull<Arena*> arena,
                   Nonnull<TraceStream*> trace_stream,
                   Nonnull<llvm::raw_ostream*> print_stream) -> ErrorOr<int>;

// Interprets `e` at compile-time, allocating values on `arena` and
// printing traces if `trace` is true. The caller must ensure that all the
// code this evaluates has been typechecked.
auto InterpExp(Nonnull<const Expression*> e, Nonnull<Arena*> arena,
               Nonnull<TraceStream*> trace_stream,
               Nonnull<llvm::raw_ostream*> print_stream)
    -> ErrorOr<Nonnull<const Value*>>;

}  // namespace Carbon

#endif  // CARBON_EXPLORER_INTERPRETER_INTERPRETER_H_
