// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// AUTOUPDATE
// RUN: %{explorer-run}
// RUN: %{explorer-run-trace}
// CHECK:STDOUT: result: 0

package ExplorerTest api;

class Point {
  private var x: i32;

  fn getX[self: Self]() -> i32 {
    return self.x;
  }
}

fn Main() -> i32 {
  var p1: Point = { .x = 1 };
  // CHECK:STDERR: COMPILATION ERROR: explorer/testdata/class/fail_access_modifier_private.carbon:[[@LINE+1]]: access_private_error
  return p1.x - 1;
}