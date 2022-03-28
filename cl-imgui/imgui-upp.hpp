#include "imgui.h"
extern "C" void imgui_text(const char *text) {
  ImGui::Text("%s", text);
}
extern "C" void imgui_text_colored(const ImVec4& col, const char* fmt) {
  ImGui::TextColored(col, "%s", fmt);
}
extern "C" void imgui_text_disabled(const char* fmt) {
  ImGui::TextDisabled("%s", fmt);
}
extern "C" void imgui_text_wrapped(const char* fmt) {
  ImGui::TextWrapped("%s", fmt);
}
extern "C" void imgui_label_text(const char *label, const char* fmt) {
  ImGui::LabelText(label, "%s", fmt);
}
extern "C" void imgui_bullet_text(const char* fmt) {
  ImGui::BulletText("%s", fmt);
}
