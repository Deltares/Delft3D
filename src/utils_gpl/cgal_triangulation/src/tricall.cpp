#include <CGAL/Constrained_Delaunay_triangulation_2.h>
#include <CGAL/Delaunay_mesh_face_base_2.h>
#include <CGAL/Delaunay_mesh_size_criteria_2.h>
#include <CGAL/Delaunay_mesher_2.h>
#include <CGAL/Delaunay_triangulation_2.h>
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Triangulation_data_structure_2.h>
#include <CGAL/Triangulation_face_base_2.h>
#include <CGAL/Triangulation_vertex_base_2.h>
#include <CGAL/Triangulation_vertex_base_with_info_2.h>
#include <CGAL/number_utils.h>

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdio>
#include <exception>
#include <map>
#include <set>
#include <utility>
#include <vector>

namespace {

using Kernel = CGAL::Exact_predicates_inexact_constructions_kernel;
using Point = Kernel::Point_2;
using Vertex_base = CGAL::Triangulation_vertex_base_with_info_2<int, Kernel>;
using Face_base = CGAL::Triangulation_face_base_2<Kernel>;
using Triangulation_data_structure = CGAL::Triangulation_data_structure_2<Vertex_base, Face_base>;
using Delaunay = CGAL::Delaunay_triangulation_2<Kernel, Triangulation_data_structure>;

using Mesh_vertex_base = CGAL::Triangulation_vertex_base_2<Kernel>;
using Mesh_face_base = CGAL::Delaunay_mesh_face_base_2<Kernel>;
using Mesh_data_structure = CGAL::Triangulation_data_structure_2<Mesh_vertex_base, Mesh_face_base>;
using Constrained_delaunay = CGAL::Constrained_Delaunay_triangulation_2<Kernel, Mesh_data_structure>;
using Mesh_criteria = CGAL::Delaunay_mesh_size_criteria_2<Constrained_delaunay>;

using Triangle = std::array<int, 3>;

Triangle rotate_to_smallest_first(Triangle triangle)
{
    const auto smallest = std::min_element(triangle.begin(), triangle.end());
    std::rotate(triangle.begin(), smallest, triangle.end());
    return triangle;
}

std::vector<Triangle> triangulate_points(const double* x, const double* y, int point_count)
{
    struct Indexed_point {
        Point point;
        int index;
    };

    std::vector<Indexed_point> points;
    points.reserve(point_count);
    for (int index = 0; index < point_count; ++index) {
        points.push_back({Point(x[index], y[index]), index + 1});
    }
    std::sort(points.begin(), points.end(), [](const Indexed_point& left, const Indexed_point& right) {
        if (left.point.x() != right.point.x()) {
            return left.point.x() < right.point.x();
        }
        if (left.point.y() != right.point.y()) {
            return left.point.y() < right.point.y();
        }
        return left.index < right.index;
    });

    Delaunay triangulation;
    for (const auto& indexed_point : points) {
        const auto previous_vertex_count = triangulation.number_of_vertices();
        const auto vertex = triangulation.insert(indexed_point.point);
        if (triangulation.number_of_vertices() != previous_vertex_count) {
            vertex->info() = indexed_point.index;
        }
    }

    std::vector<Triangle> triangles;
    triangles.reserve(triangulation.number_of_faces());
    for (auto face = triangulation.finite_faces_begin(); face != triangulation.finite_faces_end(); ++face) {
        triangles.push_back(rotate_to_smallest_first({
            face->vertex(0)->info(),
            face->vertex(1)->info(),
            face->vertex(2)->info(),
        }));
    }
    std::sort(triangles.begin(), triangles.end());
    return triangles;
}

void write_edges(const std::vector<Triangle>& triangles, int* edge_indices, int* edge_count, int* triangle_edges)
{
    std::set<std::pair<int, int>> unique_edges;
    for (const auto& triangle : triangles) {
        for (int side = 0; side < 3; ++side) {
            unique_edges.emplace(std::minmax(triangle[side], triangle[(side + 1) % 3]));
        }
    }

    std::map<std::pair<int, int>, int> edge_numbers;
    int edge_number = 1;
    for (const auto& edge : unique_edges) {
        edge_indices[2 * (edge_number - 1)] = edge.first;
        edge_indices[2 * (edge_number - 1) + 1] = edge.second;
        edge_numbers.emplace(edge, edge_number++);
    }
    *edge_count = static_cast<int>(unique_edges.size());

    for (std::size_t triangle_index = 0; triangle_index < triangles.size(); ++triangle_index) {
        for (int side = 0; side < 3; ++side) {
            const auto edge = std::minmax(
                triangles[triangle_index][side], triangles[triangle_index][(side + 1) % 3]);
            triangle_edges[3 * triangle_index + side] = edge_numbers.at(edge);
        }
    }
}

void write_triangles(const std::vector<Triangle>& triangles, int* triangle_indices)
{
    for (std::size_t triangle_index = 0; triangle_index < triangles.size(); ++triangle_index) {
        for (int vertex = 0; vertex < 3; ++vertex) {
            triangle_indices[3 * triangle_index + vertex] = triangles[triangle_index][vertex];
        }
    }
}

void generate_mesh(const double* x, const double* y, int point_count, double maximum_area,
                   std::vector<double>& mesh_x, std::vector<double>& mesh_y, std::vector<Triangle>& triangles)
{
    Constrained_delaunay triangulation;
    std::vector<Point> boundary;
    boundary.reserve(point_count);
    for (int index = 0; index < point_count; ++index) {
        boundary.emplace_back(x[index], y[index]);
    }
    triangulation.insert_constraint(boundary.begin(), boundary.end(), true);

    const double shape_bound = 0.25;
    const double size_bound = maximum_area > 0.0
        ? std::sqrt(4.0 * maximum_area / std::sqrt(3.0))
        : 0.0;
    CGAL::refine_Delaunay_mesh_2(
        triangulation, CGAL::parameters::criteria(Mesh_criteria(shape_bound, size_bound)));

    std::vector<Constrained_delaunay::Vertex_handle> vertices;
    vertices.reserve(triangulation.number_of_vertices());
    for (auto vertex = triangulation.finite_vertices_begin(); vertex != triangulation.finite_vertices_end(); ++vertex) {
        vertices.push_back(vertex);
    }
    std::sort(vertices.begin(), vertices.end(), [](const auto& left, const auto& right) {
        if (left->point().x() != right->point().x()) {
            return left->point().x() < right->point().x();
        }
        return left->point().y() < right->point().y();
    });

    std::map<const void*, int> vertex_numbers;
    mesh_x.reserve(vertices.size());
    mesh_y.reserve(vertices.size());
    for (std::size_t index = 0; index < vertices.size(); ++index) {
        vertex_numbers.emplace(static_cast<const void*>(&*vertices[index]), static_cast<int>(index) + 1);
        mesh_x.push_back(CGAL::to_double(vertices[index]->point().x()));
        mesh_y.push_back(CGAL::to_double(vertices[index]->point().y()));
    }

    for (auto face = triangulation.finite_faces_begin(); face != triangulation.finite_faces_end(); ++face) {
        if (!face->is_in_domain()) {
            continue;
        }
        triangles.push_back(rotate_to_smallest_first({
            vertex_numbers.at(static_cast<const void*>(&*face->vertex(0))),
            vertex_numbers.at(static_cast<const void*>(&*face->vertex(1))),
            vertex_numbers.at(static_cast<const void*>(&*face->vertex(2))),
        }));
    }
    std::sort(triangles.begin(), triangles.end());
}

void tricall_impl(int* mode, double* x, double* y, int* point_count, int* triangle_indices,
                  int* triangle_count, int* edge_indices, int* edge_count, int* triangle_edges,
                  double* mesh_x, double* mesh_y, int* mesh_point_count, double* maximum_area)
{
    const int triangle_capacity = *triangle_count;
    *edge_count = 0;

    try {
        if (*point_count < 3) {
            *triangle_count = 0;
            if (*mode == 2) {
                *mesh_point_count = 0;
            }
            return;
        }

        std::vector<Triangle> triangles;
        if (*mode == 1 || *mode == 3) {
            triangles = triangulate_points(x, y, *point_count);
        } else if (*mode == 2) {
            std::vector<double> generated_x;
            std::vector<double> generated_y;
            generate_mesh(x, y, *point_count, *maximum_area, generated_x, generated_y, triangles);

            const int mesh_capacity = *mesh_point_count;
            *mesh_point_count = static_cast<int>(generated_x.size());
            if (mesh_capacity < *mesh_point_count) {
                *mesh_point_count = -*mesh_point_count;
            } else {
                std::copy(generated_x.begin(), generated_x.end(), mesh_x);
                std::copy(generated_y.begin(), generated_y.end(), mesh_y);
            }
        } else {
            std::fprintf(stderr, "tricall: unsupported triangulation mode %d\n", *mode);
            *triangle_count = 0;
            return;
        }

        *triangle_count = static_cast<int>(triangles.size());
        if (triangle_capacity < *triangle_count) {
            *triangle_count = -*triangle_count;
            return;
        }

        write_triangles(triangles, triangle_indices);
        if (*mode == 3) {
            write_edges(triangles, edge_indices, edge_count, triangle_edges);
        }
    } catch (const std::exception& error) {
        std::fprintf(stderr, "tricall: CGAL triangulation failed: %s\n", error.what());
        *triangle_count = 0;
        *edge_count = 0;
        if (*mode == 2) {
            *mesh_point_count = 0;
        }
    }
}

} // namespace

#if defined(_WIN32)
extern "C" void TRICALL(int* mode, double* x, double* y, int* point_count, int* triangle_indices,
                        int* triangle_count, int* edge_indices, int* edge_count, int* triangle_edges,
                        double* mesh_x, double* mesh_y, int* mesh_point_count, double* maximum_area)
{
    tricall_impl(mode, x, y, point_count, triangle_indices, triangle_count, edge_indices, edge_count,
                 triangle_edges, mesh_x, mesh_y, mesh_point_count, maximum_area);
}
#else
extern "C" void tricall_(int* mode, double* x, double* y, int* point_count, int* triangle_indices,
                         int* triangle_count, int* edge_indices, int* edge_count, int* triangle_edges,
                         double* mesh_x, double* mesh_y, int* mesh_point_count, double* maximum_area)
{
    tricall_impl(mode, x, y, point_count, triangle_indices, triangle_count, edge_indices, edge_count,
                 triangle_edges, mesh_x, mesh_y, mesh_point_count, maximum_area);
}
#endif